{-# LANGUAGE DeriveAnyClass, DeriveGeneric, LambdaCase, OverloadedStrings #-}
module Studio.Core where

import qualified CatDB as C
import Control.DeepSeq (NFData)
import Control.Monad (unless, forM_, foldM, forM)
import Control.Monad.State.Strict (State, evalState, get, modify)
import Data.Aeson (FromJSON(..), Value, withObject, (.:), (.:?), (.!=), encode, object, (.=))
import qualified Data.ByteString.Lazy as L
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Studio.Wire
import qualified Studio.FieldService as FieldService

data Failure = Engine C.Error | Catalog C.CatalogError | InvalidInput Text
  | StaleRevision Text Text | NoDatabase | LegacyReadOnly | OperationBusy
  | Cancelled | ComputationLimit | Storage Text | Unexpected Text
  deriving (Eq, Show, Generic, NFData)

problem :: Failure -> WProblem
problem failure = WProblem code message detail
  where
    detail = case failure of
      Engine e -> TE.decodeUtf8 (L.toStrict (encode e))
      Catalog e -> TE.decodeUtf8 (L.toStrict (encode e))
      _ -> T.pack (show failure)
    (code,message) = case failure of
      Engine (C.EquationViolation _ (C.Element x)) -> ("EquationViolation",x <> " の二つの道が異なる行に着きます。関係を見直してください。")
      Engine (C.ReferenceViolation (C.Arrow a) (C.Element x) (C.Element y)) -> ("ReferenceViolation",x <> " の " <> a <> " が存在しない行 " <> y <> " を参照しています。参照を変更するか、削除を取り消してください。")
      Engine (C.FunctionDomain (C.Arrow a)) -> ("FunctionDomain",a <> " の行ごとの参照をすべて指定してください。")
      Engine (C.BudgetExceeded kind _ _) -> ("BudgetExceeded","処理量の上限に達しました（" <> T.pack (show kind) <> "）。範囲を小さくして試してください。")
      Engine (C.CyclicSchema _) -> ("CyclicSchema","この版は循環するスキーマを扱えません。矢印の向きを確認してください。")
      Engine (C.MappingEquation _) -> ("MappingEquation","この対応はスキーマのパス等式を保ちません。対応先の道を見直してください。")
      Engine _ -> ("DomainError","構造またはレコードの指定を確認してください。詳細にエンジンの判定を表示します。")
      Catalog (C.NativeError e) -> let p = problem (Engine e) in (problemCode p,problemMessage p)
      Catalog _ -> ("CatalogError","名前の重複、参照先、計算の向きを確認してください。")
      InvalidInput why -> ("InvalidInput",why)
      StaleRevision _ _ -> ("StaleRevision","確定版が更新されました。現在の版を読み直してから、変更を作り直してください。下書きは適用していません。")
      NoDatabase -> ("NoDatabase","データベースを作るか開いてください。")
      LegacyReadOnly -> ("LegacyReadOnly","このファイルにはカタログがありません。元を残し、新しいファイルに取り込むと編集できます。")
      OperationBusy -> ("Busy","別の処理を実行中です。完了を待つか取り消してください。")
      Cancelled -> ("Cancelled","処理を取り消しました。確定データは変更していません。")
      ComputationLimit -> ("TimeLimit","計算の制限時間に達しました。確定データは変更していません。")
      Storage _ -> ("StorageError","ファイルを読み書きできませんでした。保存先と詳細を確認してください。")
      Unexpected _ -> ("Unexpected","処理を完了できませんでした。詳細を確認してください。")

engine :: Either C.Error a -> Either Failure a
engine = either (Left . Engine) Right
catalog :: Either C.CatalogError a -> Either Failure a
catalog = either (Left . Catalog) Right
require :: Text -> Bool -> Either Failure ()
require message ok = unless ok (Left (InvalidInput message))
named :: Text -> Either Failure ()
named name = require "名前は空白だけにせず、200文字以内で入力してください。" (not (T.null (T.strip name)) && T.length name <= 200)
unique :: Ord a => Text -> [a] -> Either Failure ()
unique label xs = require (label <> " が重複しています。") (length xs == S.size (S.fromList xs))

-- Raw wire lists are checked before making maps: duplicates must not disappear.
fromPath :: WPath -> C.Path
fromPath (WPath o as) = C.Path (C.Object o) (map C.Arrow as)
toPath :: C.Path -> WPath
toPath (C.Path (C.Object o) as) = WPath o (map C.arrowName as)
fromSchema :: WSchema -> Either Failure C.SchemaDef
fromSchema (WSchema os as es) = do
  mapM_ named (os ++ map arrowId as)
  pure (C.SchemaDef (map C.Object os)
    [C.ArrowDef (C.Arrow a) (C.Object b) (C.Object c) | WArrow a b c <- as]
    [C.Equation (fromPath a) (fromPath b) | WEquation a b <- es])
toSchema :: C.SchemaDef -> WSchema
toSchema (C.SchemaDef os as es) = WSchema (map C.objectName os)
  [WArrow (C.arrowName a) (C.objectName b) (C.objectName c) | C.ArrowDef a b c <- as]
  [WEquation (toPath a) (toPath b) | C.Equation a b <- es]
fromData :: WData -> Either Failure C.InstanceDef
fromData tables = do
  unique "オブジェクト" (map tableObject tables)
  forM_ tables $ \(WTable _ rs) -> do
    unique "行ID" (map recordId rs)
    mapM_ (named . recordId) rs
    mapM_ (unique "矢印" . map fst . recordFields) rs
  let fields = [(C.Arrow a,(C.Element x,C.Element y)) | WTable _ rs <- tables,WRow x fs <- rs,(a,y) <- fs]
      arrowSources = M.fromListWith S.union [(a,S.singleton o) | WTable o rs <- tables,WRow _ fs <- rs,(a,_) <- fs]
  require "同じ矢印を複数のオブジェクトの行に指定しています。" (all ((==1) . S.size) (M.elems arrowSources))
  pure (C.InstanceDef (M.fromList [(C.Object o,S.fromList (map (C.Element . recordId) rs)) | WTable o rs <- tables])
    (M.fromListWith M.union [(a,M.singleton x y) | (a,(x,y)) <- fields]))
toData :: C.SchemaDef -> C.InstanceDef -> WData
toData def dat = [WTable (C.objectName o)
  [WRow (C.elementName x) [(C.arrowName (C.arrow a),maybe "" C.elementName (M.lookup (C.arrow a) (C.functions dat) >>= M.lookup x))
    | a <- C.arrows def,C.source a == o] | x <- S.toList (M.findWithDefault S.empty o (C.sets dat))] | o <- C.objects def]
checkedData :: C.Schema -> WData -> Either Failure C.Instance
checkedData schema tables = do
  dat <- fromData tables
  let def = C.definition schema
      expected o = S.fromList [C.arrowName (C.arrow a) | a <- C.arrows def,C.source a == C.Object o]
  forM_ tables $ \(WTable o rs) -> forM_ rs $ \r ->
    require (o <> " / " <> recordId r <> " の出る矢印をすべて指定してください。")
      (S.fromList (map fst (recordFields r)) == expected o)
  -- Empty source tables still have total empty functions.
  let completed = dat { C.functions = M.union (C.functions dat) (M.fromList [(C.arrow a,M.empty) | a <- C.arrows def]) }
  engine (C.instanceOfWithBudget C.defaultBudget schema completed)

data Request
  = CreateFile FilePath Text | OpenFile FilePath | AdoptLegacy FilePath Text | CloseFile
  | Preview Text Text WData | Mutate Text Mutation | Inspect Text | Explain WRef
  | CheckDefinition Text Mutation
  | Backup FilePath | Cancel
  deriving (Eq, Show)
data Mutation
  = NewSchema Text WSchema | SaveMapping WMapping
  | ApplyDraft Text Text WData Text | AdoptHead Text Text
  | RunView Text Text Text Text (Maybe Text) | Recompute Text
  | Generate Text Text Text Text
  | FieldStage Text
  deriving (Eq, Show)
instance FromJSON Request where
  parseJSON = withObject "Studio command" $ \v -> do
    tag <- v .: "tag"
    let expected = v .: "expected"
    case (tag :: Text) of
      "create" -> CreateFile <$> v .: "path" <*> v .:? "preset" .!= "blank"
      "open" -> OpenFile <$> v .: "path"
      "adoptLegacy" -> AdoptLegacy <$> v .: "path" <*> v .:? "name" .!= "imported"
      "close" -> pure CloseFile
      "preview" -> Preview <$> expected <*> v .: "source" <*> v .: "data"
      "schema" -> Mutate <$> expected <*> (NewSchema <$> v .: "name" <*> v .: "schema")
      "checkSchema" -> CheckDefinition <$> expected <*> (NewSchema <$> v .: "name" <*> v .: "schema")
      "mapping" -> Mutate <$> expected <*> (SaveMapping <$> v .: "mapping")
      "checkMapping" -> CheckDefinition <$> expected <*> (SaveMapping <$> v .: "mapping")
      "apply" -> Mutate <$> expected <*> (ApplyDraft <$> v .: "system" <*> v .: "source" <*> v .: "data" <*> v .:? "reason" .!= "Studioでレコードを編集")
      "adopt" -> Mutate <$> expected <*> (AdoptHead <$> v .: "name" <*> v .: "source")
      "run" -> Mutate <$> expected <*> (RunView <$> v .: "name" <*> v .: "operation" <*> v .: "mapping" <*> v .: "source" <*> v .:? "system")
      "recompute" -> Mutate <$> expected <*> (Recompute <$> v .: "name")
      "generate" -> Mutate <$> expected <*> (Generate <$> v .: "name" <*> v .: "source" <*> v .: "object" <*> v .: "record")
      "fieldStage" -> Mutate <$> expected <*> (FieldStage <$> v .: "stage")
      "inspect" -> Inspect <$> v .: "source"
      "explain" -> Explain <$> v .: "ref"
      "backup" -> Backup <$> v .: "path"
      "cancel" -> pure Cancel
      _ -> fail "Unknown Studio command"

fresh :: Text -> C.Catalog -> Text
fresh prefix c = choose (C.catalogRevision c + 1)
  where
    used = S.fromList (map C.unInstanceRev (M.keys (C.catalogInstances c)) ++ map C.unDerivationKey (M.keys (C.catalogDerivations c)))
    choose n = let name = prefix <> T.pack (show n) in if name `S.member` used then choose (n+1) else name

prepare :: Mutation -> C.Catalog -> Either Failure (C.Catalog,Maybe C.InstanceRev)
prepare mutation original = case mutation of
  FieldStage stage -> catalog (FieldService.runStage stage original)
  NewSchema key wire -> do
    named key
    def <- fromSchema wire
    c <- catalog (C.recordSchema C.defaultBudget (C.SchemaKey key) def original)
    s <- catalog (C.lookupSchema (C.SchemaKey key) c)
    let rev = C.InstanceRev (fresh "r" c)
    next <- catalog (C.recordInstance C.defaultBudget rev (C.SchemaKey key) (C.instanceData (C.empty s)) c)
      >>= catalog . C.recordRevisionNote rev (C.RevisionNote Nothing "空のスキーマを作成" [])
      >>= catalog . C.adoptSystem (C.SystemKey key) rev
    pure (next,Just rev)
  SaveMapping (WMapping key from to os as) -> do
    named key
    unique "オブジェクト対応" (map fst os)
    unique "矢印対応" (map fst as)
    let def = C.MappingDef (M.fromList [(C.Object a,C.Object b) | (a,b) <- os])
          (M.fromList [(C.Arrow a,fromPath p) | (a,p) <- as])
    c <- catalog (C.recordMapping C.defaultBudget (C.MappingKey key) (C.SchemaKey from) (C.SchemaKey to) def original)
    pure (c,Nothing)
  ApplyDraft system source dat reason -> do
    let rev = C.InstanceRev source; sys = C.SystemKey system
    require "選択した版は、この編集用の系の確定版ではありません。" (M.lookup sys (C.catalogHeads original) == Just rev)
    base <- catalog (C.lookupInstance rev original)
    nextData <- checkedData (C.instanceSchema base) dat
    require "変更がありません。" (nextData /= base)
    (schemaKey,_) <- maybe (Left (InvalidInput "元の版がありません。")) Right (M.lookup rev (C.catalogInstances original))
    let out = C.InstanceRev (fresh "r" original)
    recorded <- catalog (C.recordInstance C.defaultBudget out schemaKey (C.instanceData nextData) original)
    -- These shipped fixture anchors name display payloads, not inferred identities.
    let retained = [(C.RowRef out o x,a) | (C.RowRef old o x,anchors) <- M.toList (C.catalogBindings original),old==rev
                   ,x `S.member` M.findWithDefault S.empty o (C.sets (C.instanceData nextData)),a <- S.toList anchors,a `M.member` fixtureLabels]
    labelled <- foldM (\c (r,a) -> catalog (C.bindAnchor r a c)) recorded retained
    c <- catalog (C.recordRevisionNote out (C.RevisionNote (Just rev) reason []) labelled)
      >>= catalog . C.adoptSystem sys out
    pure (c,Just out)
  AdoptHead key source -> do
    named key
    c <- catalog (C.adoptSystem (C.SystemKey key) (C.InstanceRev source) original)
    pure (c,Just (C.InstanceRev source))
  RunView key mode mapping source system -> do
    named key
    require "そのビュー名は既に使われています。再計算を使うか、別の名前を選んでください。" (M.notMember (C.ViewKey key) (C.catalogViews original))
    spec <- case mode of
      "delta" -> Right (C.PullAlong (C.MappingKey mapping))
      "sigma" -> Right (C.PushAlong (C.MappingKey mapping))
      "pi" -> Right (C.JoinAlong (C.MappingKey mapping))
      _ -> Left (InvalidInput "計算は delta / sigma / pi から選んでください。")
    let binding = maybe (C.Pinned (C.InstanceRev source)) (C.Active . C.SystemKey) system
    forM_ system $ \sys -> require "計算元の確定版が変わりました。" (M.lookup (C.SystemKey sys) (C.catalogHeads original) == Just (C.InstanceRev source))
    run key binding (C.InstanceRev source) spec original
  Recompute key -> do
    adoption <- maybe (Left (InvalidInput "ビューが見つかりません。")) Right (M.lookup (C.ViewKey key) (C.catalogViews original))
    d <- catalog (C.lookupDerivation (C.adoptionDerivation adoption) original)
    source <- case C.adoptionBinding adoption of
      C.Pinned rev -> pure rev
      C.Active sys -> maybe (Left (InvalidInput "元の系がありません。")) Right (M.lookup sys (C.catalogHeads original))
    -- Studio views have one explicit input. Imported multi-step views stay intact.
    require "このビューは複数段の由来を持ちます。定義の各段を確認し、新しいビューとして計算してください。"
      (C.adoptionSourceRevision adoption == C.derivationInput d)
    run key (C.adoptionBinding adoption) source (C.derivationSpec d) original
  Generate key source o x -> do
    named key
    require "そのビュー名は既に使われています。" (M.notMember (C.ViewKey key) (C.catalogViews original))
    run key (C.Pinned (C.InstanceRev source)) (C.InstanceRev source)
      (C.Generate (M.singleton (C.Object o) (S.singleton (C.Element x)))) original
  where
    run key binding source spec c = do
      let out = C.InstanceRev (fresh "r" c); dk = C.DerivationKey (fresh "d" c)
      next <- catalog (C.derive C.defaultBudget dk out spec source c)
        >>= catalog . C.adoptView C.defaultBudget (C.ViewKey key) binding dk
      pure (next,Just out)

snapshotWire :: C.InstanceRev -> C.Catalog -> Either Failure WSnapshot
snapshotWire rev c = do
  (sk,_) <- maybe (Left (InvalidInput "版が見つかりません。")) Right (M.lookup rev (C.catalogInstances c))
  i <- catalog (C.lookupInstance rev c)
  let def = C.definition (C.instanceSchema i)
  pure (WSnapshot (C.unInstanceRev rev) (C.unSchemaKey sk) (toSchema def) (toData def (C.instanceData i))
    (any ((==rev) . C.derivationOutput) (M.elems (C.catalogDerivations c))) (captionEntries rev i c))

fixtureAnchors :: [(C.Object,C.Element,C.AnchorId,Text)]
fixtureAnchors = [(o,x,C.AnchorId ("catdb-studio:reading:"<>C.objectName o<>":"<>C.elementName x),label)
  | (o,x,label) <- [("Note","n1","帰る場所について"),("Note","n2","雨の日の過ごし方"),("Note","n3","小さな灯りの役割")
    ,("Book","b1","遠い灯台"),("Book","b2","雨の庭"),("Quote","q1","帰る場所は、歩くうちに見つかる。")
    ,("Quote","q2","雨の音を聞くために、窓を少し開けた。"),("Quote","q3","灯りがひとつあれば、夜は同じではない。")]]
fixtureLabels :: M.Map C.AnchorId Text
fixtureLabels = M.fromList [(anchor,label) | (_,_,anchor,label) <- fixtureAnchors]

captionEntries :: C.InstanceRev -> C.Instance -> C.Catalog -> [(Text,[(Text,Text)])]
captionEntries rev instanceValue c = evalState (forM (M.toList (C.sets (C.instanceData instanceValue))) $ \(o,xs) -> do
  labels <- forM (S.toList xs) $ \x -> do
    caption <- labelFor (C.RowRef rev o x)
    pure ((,) (C.elementName x) <$> caption)
  pure (C.objectName o,catMaybes labels)) M.empty
  where
    labelFor :: C.RowRef -> State (M.Map C.RowRef (Maybe Text)) (Maybe Text)
    labelFor row = do
      cache <- get
      case M.lookup row cache of
        Just found -> pure found
        Nothing -> do
          modify (M.insert row Nothing)
          caption <- case C.explainRow row c of
            Left _ -> pure Nothing
            Right e -> case C.rowDerivation e of
              Nothing -> pure (case catMaybes [M.lookup a fixtureLabels | a <- C.directAnchors e] of [one] -> Just one; _ -> Nothing)
              Just (_,C.PullRef origin) -> labelFor origin
              Just (_,C.GeneratedRef origin) -> labelFor origin
              Just (_,C.SigmaClass origins) -> case fieldIdentity row origins of
                Just source->pure (Just (C.elementName (C.rowElement source)))
                Nothing->summary "件の由来" (map C.transportRow origins)
              Just (_,C.PiFamily []) -> pure (Just "空の組")
              Just (_,C.PiFamily origins) -> case fieldMember row origins of
                Just source->labelFor source
                Nothing->summary "成分の組" (map C.memberRow origins)
          modify (M.insert row caption)
          pure caption
    summary unit origins = do
      names <- forM (take 3 origins) $ \row -> do
        name <- labelFor row
        pure (maybe (C.objectName (C.rowObject row)<>"/"<>C.elementName (C.rowElement row)) id name)
      pure (Just (T.pack (show (length origins))<>unit<>" · "<>T.intercalate " / " names<>(if length origins>3 then " / …" else "")))
    fieldRow row=maybe False (T.isPrefixOf "fs:" . C.unSchemaKey . fst) (M.lookup (C.rowRevision row) (C.catalogInstances c))
    fieldIdentity row origins
      | not (fieldRow row)=Nothing
      | otherwise=case [C.transportRow origin | origin<-origins,null (C.steps (C.transportPath origin))
          ,C.rowObject (C.transportRow origin)==(if C.rowObject row=="顧客" then "顧客キー" else C.rowObject row)] of [one]->Just one; _->Nothing
    fieldMember row origins
      | not (fieldRow row)=Nothing
      | otherwise=case [C.memberRow member | member<-origins,C.memberObject member==C.rowObject row,null (C.steps (C.memberPath member))] of [one]->Just one; _->Nothing

explanationWire :: WRef -> C.Catalog -> Either Failure WExplanation
explanationWire (WRef r o x) c = do
  explained <- catalog (C.explainRow (C.RowRef (C.InstanceRev r) (C.Object o) (C.Element x)) c)
  let ref (C.RowRef a b z) = WRef (C.unInstanceRev a) (C.objectName b) (C.elementName z)
      simple a = WOrigin (ref a) Nothing Nothing
      (kind,origins) = case C.rowDerivation explained of
        Nothing -> ("source",[])
        Just (_,C.PullRef a) -> ("PullRef",[simple a])
        Just (_,C.GeneratedRef a) -> ("GeneratedRef",[simple a])
        Just (_,C.SigmaClass ts) -> ("SigmaClass",[WOrigin (ref a) (Just (toPath p)) Nothing | C.Transport p a <- ts])
        Just (_,C.PiFamily ms) -> ("PiFamily",[WOrigin (ref a) (Just (toPath p)) (Just (C.objectName b)) | C.FamilyMember b p a <- ms])
  pure (WExplanation (ref (C.explainedRow explained)) kind origins (map C.unAnchorId (C.directAnchors explained)))

catalogWire :: Text -> Maybe Text -> Bool -> C.Catalog -> WState
catalogWire version path busy c = WState version path
  [(C.unSystemKey k,C.unInstanceRev r) | (k,r) <- M.toList (C.catalogHeads c)]
  [(C.unSchemaKey k,toSchema s) | (k,s) <- M.toList (C.catalogSchemas c)]
  [WRevision (C.unInstanceRev r) (C.unSchemaKey sk) (r `S.member` derived)
    (C.unInstanceRev <$> (note >>= C.revisionPrevious)) (maybe "" C.revisionReason note)
    (sum (map S.size (M.elems (C.sets dat))))
    | (r,(sk,dat)) <- M.toList (C.catalogInstances c),let note = M.lookup r (C.catalogRevisionNotes c)]
  [WMapping (C.unMappingKey k) (C.unSchemaKey a) (C.unSchemaKey b)
    [(C.objectName x,C.objectName y) | (x,y) <- M.toList (C.onObjects def)]
    [(C.arrowName x,toPath p) | (x,p) <- M.toList (C.onArrows def)]
    | (k,(a,b,def)) <- M.toList (C.catalogMappings c)]
  [WView (C.unViewKey k) mode mapping (C.unInstanceRev (C.derivationInput d)) (C.unInstanceRev (C.derivationOutput d))
    (case C.adoptionBinding adoption of C.Active sys -> Just (C.unSystemKey sys); _ -> Nothing)
    (case C.viewFreshness C.defaultBudget k c of Right C.Current -> "current"; Right C.PinnedSnapshot -> "pinned"; Right (C.Stale _ _) -> "stale"; Left _ -> "unavailable")
    (Just (C.unInstanceRev (C.adoptionSourceRevision adoption)))
    | (k,adoption) <- M.toList (C.catalogViews c),Just d <- [M.lookup (C.adoptionDerivation adoption) (C.catalogDerivations c)]
    ,let (mode,mapping) = case C.derivationSpec d of C.PullAlong m -> ("delta",C.unMappingKey m); C.PushAlong m -> ("sigma",C.unMappingKey m); C.JoinAlong m -> ("pi",C.unMappingKey m); C.Generate _ -> ("generated","")]
  Nothing busy (FieldService.status c)
  where derived = S.fromList (map C.derivationOutput (M.elems (C.catalogDerivations c)))

exportCatalog :: C.Catalog -> Value
exportCatalog c = object
  ["format" .= ("catdb-studio-definitions-v1" :: Text),"budget" .= C.defaultBudget
  ,"schemas" .= C.catalogSchemas c,"instances" .= C.catalogInstances c,"mappings" .= C.catalogMappings c
  ,"heads" .= C.catalogHeads c,"views" .= C.catalogViews c,"derivations" .= C.catalogDerivations c
  ,"revisionNotes" .= C.catalogRevisionNotes c,"bindings" .= C.catalogBindings c]

initialCatalog :: Text -> Either Failure C.Catalog
initialCatalog preset = case preset of
  "blank" -> pure C.emptyCatalog
  "reading" -> readingCatalog
  "families" -> familyCatalog
  "field-service" -> catalog FieldService.seed
  _ -> Left (InvalidInput "見本は field-service / reading / families / blank から選んでください。")

readingCatalog :: Either Failure C.Catalog
readingCatalog = do
  let reading = C.SchemaDef ["Note","Quote","Book"]
        [C.ArrowDef "book" "Note" "Book",C.ArrowDef "quote" "Note" "Quote",C.ArrowDef "quoteBook" "Quote" "Book"]
        [C.Equation (C.Path "Note" ["book"]) (C.Path "Note" ["quote","quoteBook"])]
      cards = C.SchemaDef ["Card","Book"] [C.ArrowDef "volume" "Card" "Book"] []
      dat = C.InstanceDef (M.fromList [("Note",S.fromList ["n1","n2","n3"]),("Quote",S.fromList ["q1","q2","q3"]),("Book",S.fromList ["b1","b2"])])
        (M.fromList [("book",M.fromList [("n1","b1"),("n2","b2"),("n3","b1")]),("quote",M.fromList [("n1","q1"),("n2","q2"),("n3","q3")]),("quoteBook",M.fromList [("q1","b1"),("q2","b2"),("q3","b1")])])
  c <- catalog (C.recordSchema C.defaultBudget "reading" reading C.emptyCatalog)
    >>= catalog . C.recordSchema C.defaultBudget "cards" cards
    >>= catalog . C.recordInstance C.defaultBudget "r1" "reading" dat
    >>= catalog . C.adoptSystem "reading" "r1"
    >>= catalog . C.recordRevisionNote "r1" (C.RevisionNote Nothing "架空の読書メモ見本" [])
    >>= catalog . C.recordMapping C.defaultBudget "cards" "cards" "reading"
      (C.MappingDef (M.fromList [("Card","Note"),("Book","Book")]) (M.singleton "volume" (C.Path "Note" ["quote","quoteBook"])))
  foldM (\acc (o,x,a,_) -> catalog (C.bindAnchor (C.RowRef "r1" o x) a acc)) c fixtureAnchors

familyCatalog :: Either Failure C.Catalog
familyCatalog = do
  let spanSchema = C.SchemaDef ["Left","Right","Key"] [C.ArrowDef "leftKey" "Left" "Key",C.ArrowDef "rightKey" "Right" "Key"] []
      one = C.SchemaDef ["Result"] [] []
      dat = C.InstanceDef (M.fromList [("Left",S.fromList ["left-A","left-B"]),("Right",S.fromList ["right-A","right-B"]),("Key",S.fromList ["A","B"])])
        (M.fromList [("leftKey",M.fromList [("left-A","A"),("left-B","B")]),("rightKey",M.fromList [("right-A","A"),("right-B","B")])])
  catalog (C.recordSchema C.defaultBudget "families" spanSchema C.emptyCatalog)
    >>= catalog . C.recordSchema C.defaultBudget "result" one
    >>= catalog . C.recordInstance C.defaultBudget "r1" "families" dat
    >>= catalog . C.adoptSystem "families" "r1"
    >>= catalog . C.recordRevisionNote "r1" (C.RevisionNote Nothing "同じキーで閉じる組と、宣言した同一視の見本" [])
    >>= catalog . C.recordMapping C.defaultBudget "by-key" "families" "result"
      (C.MappingDef (M.fromList [("Left","Result"),("Right","Result"),("Key","Result")])
        (M.fromList [("leftKey",C.Path "Result" []),("rightKey",C.Path "Result" [])]))
