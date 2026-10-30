{-# LANGUAGE DeriveAnyClass, DeriveGeneric, LambdaCase, OverloadedStrings, ScopedTypeVariables #-}
module Studio.Runtime (Runtime, newRuntime, shutdown, readState, submit, nativeBackup, definitionExport) where

import qualified CatDB as C
import Control.Concurrent.Async (Async, async, cancel, waitCatch)
import Control.Concurrent.MVar
import Control.DeepSeq (NFData, force)
import Control.Exception (IOException, evaluate, try, displayException, mask, onException)
import Control.Monad (void)
import Data.Aeson (Value)
import qualified Data.ByteString as B
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.Directory (makeAbsolute, createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory, takeExtension, isAbsolute, normalise)
import System.Timeout (timeout)
import Studio.Core
import Studio.Wire

data Opened = Opened
  { openedPath :: FilePath, openedDatabase :: C.Database, openedCatalog :: Maybe C.Catalog
  , openedExternal :: Maybe C.Instance, openedLegacy :: Maybe C.Instance }
data Work = Persist C.Catalog C.Instance (Maybe C.InstanceRev) | Previewed C.InstanceRev C.Instance
  deriving (Generic, NFData)
data Session = Session
  { generation :: Integer, jobCounter :: Integer, opened :: Maybe Opened
  , running :: Maybe (Integer,Async (Either Failure Work)) }
data Runtime = Runtime FilePath (MVar Session)

newRuntime :: FilePath -> IO Runtime
newRuntime dataRoot = Runtime <$> makeAbsolute dataRoot <*> newMVar (Session 0 0 Nothing Nothing)

version :: Session -> Text
version s = T.pack (show (generation s)) <> ":" <> maybe "closed" (maybe "legacy" (T.pack . show . C.catalogRevision) . openedCatalog) (opened s)

sessionState :: Session -> WState
sessionState s = case opened s of
  Nothing -> WState (version s) Nothing [] [] [] [] [] Nothing (isJust (running s)) Nothing
  Just o -> case openedCatalog o of
    Just c -> catalogWire (version s) (Just (T.pack (openedPath o))) (isJust (running s)) c
    Nothing -> WState (version s) (Just (T.pack (openedPath o))) [] [] [] [] []
      (fmap (\i -> let def = C.definition (C.instanceSchema i) in WSnapshot "legacy" "legacy" (toSchema def) (toData def (C.instanceData i)) False []) (openedLegacy o)) False Nothing

readState :: Runtime -> IO WState
readState (Runtime _ cell) = withMVar cell (pure . sessionState)

failureReply :: Failure -> WReply
failureReply e = WReply False (problemMessage p) (Just p) Nothing Nothing Nothing where p = problem e
success :: Text -> WReply
success msg = WReply True msg Nothing Nothing Nothing Nothing

withIOFailure :: forall a. IO (Either Failure a) -> IO (Either Failure a)
withIOFailure action = do
  result <- try action
  pure (either (Left . Storage . T.pack . displayException) id (result :: Either IOException (Either Failure a)))

filePath :: FilePath -> FilePath -> IO (Either Failure FilePath)
filePath root input
  | null input = pure (Left (InvalidInput "保存先のファイル名を入力してください。"))
  | otherwise = do
      absolute <- makeAbsolute (normalise (if isAbsolute input then input else root </> input))
      pure $ if T.toLower (T.pack (takeExtension absolute)) /= ".catdb"
        then Left (InvalidInput "ファイル名の末尾は .catdb にしてください。") else Right absolute

storeResult :: Either C.StoreError a -> Either Failure a
storeResult = either (Left . Storage . T.pack . show) Right

closeOpened :: Maybe Opened -> IO (Either Failure ())
closeOpened Nothing = pure (Right ())
closeOpened (Just o) = storeResult <$> C.closeDatabase (openedDatabase o)

loadOpened :: FilePath -> IO (Either Failure Opened)
loadOpened path = do
  acquired <- C.openDatabaseWithBudget C.defaultBudget path
  case storeResult acquired of
    Left e -> pure (Left e)
    Right db -> do
      result <- C.snapshot db
      case storeResult result >>= classify db of
        Left e -> void (C.closeDatabase db) >> pure (Left e)
        Right o -> pure (Right o)
  where
    classify db i
      | any (T.isPrefixOf "catdb.catalog." . C.objectName) (C.objects (C.definition (C.instanceSchema i))) = do
          c <- catalog (C.projectCatalog C.defaultBudget i)
          let def = C.definition (C.instanceSchema i)
              applicationDef = C.SchemaDef
                (filter (not . T.isPrefixOf "catdb.catalog." . C.objectName) (C.objects def))
                (filter (not . T.isPrefixOf "catdb.catalog." . C.arrowName . C.arrow) (C.arrows def))
                (filter (not . T.isPrefixOf "catdb.catalog." . C.objectName . C.start . C.left) (C.equations def))
          external <- if null (C.objects applicationDef) then pure Nothing else do
            s <- engine (C.schemaWithBudget C.defaultBudget applicationDef)
            Just <$> catalog (C.projectInstance C.defaultBudget s i)
          pure (Opened path db (Just c) external Nothing)
      | otherwise = Right (Opened path db Nothing Nothing (Just i))

createOpened :: FilePath -> C.Catalog -> IO (Either Failure Opened)
createOpened path c = case catalog (C.encodeCatalog C.defaultBudget c) of
  Left e -> pure (Left e)
  Right i -> do
    createDirectoryIfMissing True (takeDirectory path)
    result <- C.createDatabase path i
    pure (fmap (\db -> Opened path db (Just c) Nothing Nothing) (storeResult result))

shutdown :: Runtime -> IO ()
shutdown (Runtime _ cell) = mask $ \_ -> do
  task <- modifyMVar cell $ \s -> pure (s {running=Nothing},snd <$> running s)
  mapM_ cancel task
  modifyMVar_ cell $ \s -> closeOpened (opened s) >> pure s {opened=Nothing,generation=generation s+1}

submit :: Runtime -> Request -> IO WReply
submit runtime@(Runtime root cell) request = do
  result <- case request of
    Mutate expected mutation -> work runtime expected $ \o c -> do
      (next,selected) <- prepare mutation c
      native <- catalog (C.encodeCatalog C.defaultBudget next)
      encoded <- maybe (Right native) (\external -> catalog (C.combineInstances C.defaultBudget external native)) (openedExternal o)
      pure (Persist next encoded selected)
    Preview expected source dat -> work runtime expected $ \_ c -> do
      base <- catalog (C.lookupInstance (C.InstanceRev source) c)
      next <- checkedData (C.instanceSchema base) dat
      pure (Previewed (C.InstanceRev source) next)
    Cancel -> do
      task <- modifyMVar cell $ \s -> pure (s {running=Nothing},snd <$> running s)
      case task of
        Nothing -> pure (failureReply (InvalidInput "取り消す計算はありません。"))
        Just a -> cancel a >> pure (success "計算を取り消しました。")
    Inspect key -> withMVar cell $ \s -> pure $ case opened s of
      Just o | Just c <- openedCatalog o -> either failureReply (\snap -> (success "版を開きました。") {replySnapshot=Just snap}) (snapshotWire (C.InstanceRev key) c)
      Just o | Just i <- openedLegacy o -> pureLegacy i
      _ -> failureReply NoDatabase
    Explain ref -> withMVar cell $ \s -> pure $ case opened s >>= openedCatalog of
      Nothing -> failureReply NoDatabase
      Just c -> either failureReply (\e -> (success "由来を読みました。") {replyExplanation=Just e}) (explanationWire ref c)
    Backup input -> do
      target <- filePath root input
      case target of
        Left e -> pure (failureReply e)
        Right path -> withMVar cell $ \s -> case opened s of
          Nothing -> pure (failureReply NoDatabase)
          Just o -> do
            result <- C.copyDatabase (openedDatabase o) path
            pure (either failureReply (const (success ("バックアップを保存しました：" <> T.pack path))) (storeResult result))
    _ -> do
      result <- withIOFailure $ modifyMVar cell $ \s ->
        if isJust (running s) then pure (s,Left OperationBusy) else lifecycle root s request
      pure (either failureReply id result)
  current <- readState runtime
  pure result {replyState=Just current}
  where
    pureLegacy i = let def = C.definition (C.instanceSchema i) in
      (success "元のファイルを読み取り用に開きました。") {replySnapshot=Just (WSnapshot "legacy" "legacy" (toSchema def) (toData def (C.instanceData i)) False [])}

lifecycle :: FilePath -> Session -> Request -> IO (Session,Either Failure WReply)
lifecycle root s request = case request of
  CloseFile -> do
    result <- closeOpened (opened s)
    pure (s {opened=Nothing,generation=generation s+1},success "ファイルを閉じました。" <$ result)
  OpenFile input -> do
    target <- filePath root input
    case target of
      Left e -> pure (s,Left e)
      Right path | maybe False ((==path) . openedPath) (opened s) -> pure (s,Right (success "このファイルは既に開いています。"))
      Right path -> loadOpened path >>= replace
  CreateFile input preset -> do
    target <- filePath root input
    case (target,initialCatalog preset) of
      (Right path,Right c) -> createOpened path c >>= replace
      (Left e,_) -> pure (s,Left e)
      (_,Left e) -> pure (s,Left e)
  AdoptLegacy input name -> case opened s >>= openedLegacy of
    Nothing -> pure (s,Left (InvalidInput "取り込み元のファイルを先に開いてください。"))
    Just i -> do
      target <- filePath root input
      let adopted = do
            named name
            c <- catalog (C.recordSchema C.defaultBudget (C.SchemaKey name) (C.definition (C.instanceSchema i)) C.emptyCatalog)
              >>= catalog . C.recordInstance C.defaultBudget "r1" (C.SchemaKey name) (C.instanceData i)
              >>= catalog . C.recordRevisionNote "r1" (C.RevisionNote Nothing "既存ファイルを元を残して取り込み" [])
              >>= catalog . C.adoptSystem (C.SystemKey name) "r1"
            pure c
      case (target,adopted) of
        (Right path,Right c) -> createOpened path c >>= replace
        (Left e,_) -> pure (s,Left e)
        (_,Left e) -> pure (s,Left e)
  _ -> pure (s,Left (InvalidInput "この操作はここでは実行できません。"))
  where
    replace (Left e) = pure (s,Left e)
    replace (Right next) = do
      closed <- closeOpened (opened s)
      case closed of
        Left e -> void (C.closeDatabase (openedDatabase next)) >> pure (s {opened=Nothing},Left e)
        Right () -> do
          recovered <- C.recoveredBytes (openedDatabase next)
          let suffix = case recovered of Right n | n>0 -> "（未完了の末尾 " <> T.pack (show n) <> " バイトをストアの規則で回復）"; _ -> ""
              msg = if isJust (openedLegacy next) then "既存ファイルを読み取り用に開きました。編集には別ファイルへの取り込みが必要です。" else "ファイルを開きました。"
          pure (s {opened=Just next,generation=generation s+1},Right (success (msg <> suffix)))

work :: Runtime -> Text -> (Opened -> C.Catalog -> Either Failure Work) -> IO WReply
work (Runtime _ cell) expected compute = mask $ \restore -> do
  admitted <- modifyMVar cell $ \s -> case opened s of
    Nothing -> pure (s,Left NoDatabase)
    Just o | isJust (running s) -> pure (s,Left OperationBusy)
           | expected /= version s -> pure (s,Left (StaleRevision expected (version s)))
           | Nothing <- openedCatalog o -> pure (s,Left LegacyReadOnly)
           | Just c <- openedCatalog o -> do
               let job = jobCounter s + 1
               a <- async $ restore $ do
                 result <- timeout 20000000 (evaluate (force (compute o c)))
                 pure (maybe (Left ComputationLimit) id result)
               pure (s {jobCounter=job,running=Just (job,a)},Right (job,a))
  case admitted of
    Left e -> pure (failureReply e)
    Right (job,a) -> do
      result <- restore (waitCatch a) `onException` do
        cancel a
        modifyMVar_ cell $ \s -> pure (if maybe False ((==job) . fst) (running s) then s {running=Nothing} else s)
      modifyMVar cell $ \s -> if maybe True ((/=job) . fst) (running s)
        then pure (s,failureReply Cancelled)
        else do
          let done = s {running=Nothing}
              computed = either (Left . Unexpected . T.pack . displayException) id result
          case computed of
            Left e -> pure (done,failureReply e)
            Right _ | expected /= version s -> pure (done,failureReply (StaleRevision expected (version s)))
            Right (Previewed rev i) ->
              let def = C.definition (C.instanceSchema i)
                  labels = maybe [] (captionEntries rev i) (opened s >>= openedCatalog)
                  snap = WSnapshot (C.unInstanceRev rev) "preview" (toSchema def) (toData def (C.instanceData i)) False labels
              in pure (done,(success "すべての参照と二つの道の約束を確認しました。まだ保存していません。") {replySnapshot=Just snap})
            Right (Persist c encoded selected) -> case opened s of
              Nothing -> pure (done,failureReply NoDatabase)
              Just o -> do
                committed <- C.transaction (openedDatabase o) (\_ -> Right ((),encoded))
                case storeResult committed of
                  Left e -> pure (done,failureReply e)
                  Right () -> pure (done {opened=Just o {openedCatalog=Just c}},
                    (success "新しい確定版を保存しました。") {replySnapshot=selected >>= either (const Nothing) Just . (\rev -> snapshotWire rev c)})

nativeBackup :: Runtime -> IO (Either Failure B.ByteString)
nativeBackup (Runtime _ cell) = withMVar cell $ \s -> case opened s of
  Nothing -> pure (Left NoDatabase)
  Just o -> storeResult <$> C.backupBytes (openedDatabase o)

definitionExport :: Runtime -> IO (Either Failure Value)
definitionExport (Runtime _ cell) = withMVar cell $ \s -> pure $ case opened s >>= openedCatalog of
  Nothing -> Left NoDatabase
  Just c -> Right (exportCatalog c)
