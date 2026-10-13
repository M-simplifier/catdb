{-# LANGUAGE DeriveAnyClass, ForeignFunctionInterface, ScopedTypeVariables #-}
module CatDB.Store
  ( Database, StoreError(..), createDatabase, openDatabase, closeDatabase
  , withDatabase, snapshot, transaction, commit, copyDatabase, backupBytes, recoveredBytes
  ) where

import CatDB.Internal
import CatDB.Schema (schema)
import CatDB.Instance (instanceOf, edit, Edit)
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad (when)
import Data.Binary (Binary, encode, decodeOrFail)
import Data.Binary.Get (runGet, getByteString, getWord64le)
import Data.Binary.Put
import Data.Bits (complement, xor)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy as L
import Data.Int (Int64)
import Data.List (foldl')
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word (Word8, Word64)
import Foreign (Ptr, alloca, peek, castPtr)
import Foreign.C
import GHC.Generics (Generic)

data StoreError = DomainError Error | Busy | Closed | ReopenRequired
  | Corrupt Integer Text | NativeFailure Text Int | FileError Text
  | CommitUncertain Text
  deriving (Eq, Show)
newtype StoreException = StoreException StoreError deriving (Show)
instance Exception StoreException

data Open = Open { descriptor :: CInt, current :: Instance, endOffset :: Int64
                 , recovered :: Int64, poisoned :: Bool }
newtype Database = Database (MVar (Maybe Open))

data Change = Add Object Element | Remove Object Element
            | Set Arrow Element Element | Unset Arrow Element
  deriving stock (Generic)
  deriving anyclass (Binary)
data Record = Snapshot SchemaDef InstanceDef | Changes [Change]
  deriving stock (Generic)
  deriving anyclass (Binary)

foreign import ccall safe "catdb_open" c_open :: CString -> CInt -> Ptr CInt -> IO CInt
foreign import ccall safe "catdb_close" c_close :: CInt -> IO CInt
foreign import ccall safe "catdb_seek" c_seek :: CInt -> Int64 -> IO CInt
foreign import ccall safe "catdb_size" c_size :: CInt -> Ptr Int64 -> IO CInt
foreign import ccall safe "catdb_read" c_read :: CInt -> Ptr Word8 -> CSize -> IO CInt
foreign import ccall safe "catdb_write" c_write :: CInt -> Ptr Word8 -> CSize -> IO CInt
foreign import ccall safe "catdb_sync" c_sync :: CInt -> IO CInt
foreign import ccall safe "catdb_truncate" c_truncate :: CInt -> Int64 -> IO CInt

failStore :: StoreError -> IO a
failStore = throwIO . StoreException

capture :: IO a -> IO (Either StoreError a)
capture action = (Right <$> action) `catches`
  [Handler (\(StoreException e) -> pure (Left e))
  ,Handler (\(e :: IOException) -> pure (Left (FileError (T.pack (displayException e)))))]

native :: Text -> IO CInt -> IO ()
native label action = do
  code <- action
  when (code /= 0) (failStore (NativeFailure label (fromIntegral code)))

getHandle :: FilePath -> Bool -> IO CInt
getHandle path create = do
  when ('\0' `elem` path) (failStore (FileError "NUL in file path"))
  B.useAsCString (T.encodeUtf8 (T.pack path)) $ \p -> alloca $ \err -> do
    fd <- c_open p (if create then 1 else 0) err
    when (fd < 0) $ do
      e <- peek err
      failStore (if e == -1 then Busy else NativeFailure "open" (fromIntegral e))
    pure fd

closeHandle :: CInt -> IO ()
closeHandle fd = native "close" (c_close fd)

readBytes :: CInt -> Int -> IO B.ByteString
readBytes fd n = BI.create n (\p -> native "read" (c_read fd p (fromIntegral n)))

writeBytes :: CInt -> B.ByteString -> IO ()
writeBytes fd bs = B.useAsCStringLen bs $ \(p,n) -> native "write" (c_write fd (castPtr p) (fromIntegral n))

checksum :: B.ByteString -> Word64
checksum = B.foldl' (\h b -> (h `xor` fromIntegral b) * 1099511628211) 14695981039346656037

-- Format version is in the header. Incomplete final frames are uncommitted.
-- A complete but invalid frame is corruption, never silently discarded.
frame :: Record -> B.ByteString
frame record = L.toStrict (runPut $ do
  putByteString "CATTX001"
  putWord64le n
  putWord64le (complement n)
  putByteString payload
  putWord64le (checksum payload)
  putByteString "COMMIT01")
  where payload = L.toStrict (encode record); n = fromIntegral (B.length payload)

appendFrame :: CInt -> Int64 -> B.ByteString -> IO Int64
appendFrame fd offset bytes = do
  when (B.length bytes > maxPayload + 40) (failStore (FileError "transaction exceeds 256 MiB"))
  native "seek" (c_seek fd offset)
  writeBytes fd bytes
  native "durable flush" (c_sync fd)
  pure (offset + fromIntegral (B.length bytes))

maxPayload :: Int
maxPayload = 256 * 1024 * 1024

createDatabase :: FilePath -> Instance -> IO (Either StoreError Database)
createDatabase path initial = capture $ mask_ $
  bracketOnError (getHandle path True) closeHandle $ \fd -> do
    offset <- appendFrame fd 0 (frame (Snapshot (definition (instanceSchema initial)) (instanceData initial)))
    Database <$> newMVar (Just (Open fd initial offset 0 False))

openDatabase :: FilePath -> IO (Either StoreError Database)
openDatabase path = capture $ mask_ $
  bracketOnError (getHandle path False) closeHandle $ \fd -> do
    size <- alloca $ \p -> native "size" (c_size fd p) >> peek p
    (s,dat,offset) <- replay fd size 0 Nothing
    i <- either (failStore . DomainError) pure (instanceOf s dat)
    when (offset < size) $ do
      native "recover incomplete tail" (c_truncate fd offset)
      native "flush recovery" (c_sync fd)
    Database <$> newMVar (Just (Open fd i offset (size-offset) False))

replay :: CInt -> Int64 -> Int64 -> Maybe (Schema,InstanceDef) -> IO (Schema,InstanceDef,Int64)
replay fd size offset previous
  | size-offset < 24 = finish
  | otherwise = do
      bytes <- readBytes fd 24
      let (magic,n,inverse) = runGet ((,,) <$> getByteString 8 <*> getWord64le <*> getWord64le) (L.fromStrict bytes)
      when (magic /= "CATTX001" || inverse /= complement n || n > fromIntegral maxPayload)
        (bad "invalid frame header, version, or length")
      if toInteger n + 40 > toInteger (size-offset) then finish else do
        payload <- readBytes fd (fromIntegral n)
        footer <- readBytes fd 16
        let (sum',marker) = runGet ((,) <$> getWord64le <*> getByteString 8) (L.fromStrict footer)
        when (sum' /= checksum payload || marker /= "COMMIT01") (bad "frame checksum or commit marker mismatch")
        record <- case decodeOrFail (L.fromStrict payload) of
          Left (_,_,msg) -> bad (T.pack msg)
          Right (rest,_,value) | L.null rest -> pure value
                              | otherwise -> bad "trailing record bytes"
        next <- case record of
          Snapshot def dat -> do
            s <- either (failStore . DomainError) pure (schema def)
            pure (s,dat)
          Changes changes -> case previous of
            Nothing -> bad "first record is not a snapshot"
            Just (s,dat) -> pure (s,foldl' applyChange dat changes)
        replay fd size (offset + fromIntegral n + 40) (Just next)
  where
    bad msg = failStore (Corrupt (toInteger offset) msg)
    finish = case previous of
      Nothing -> bad "no committed snapshot"
      Just (s,dat) -> pure (s,dat,offset)

applyChange :: InstanceDef -> Change -> InstanceDef
applyChange d c = case c of
  Add o x -> d { sets = M.adjust (S.insert x) o (sets d) }
  Remove o x -> d { sets = M.adjust (S.delete x) o (sets d) }
  Set a x y -> d { functions = M.adjust (M.insert x y) a (functions d) }
  Unset a x -> d { functions = M.adjust (M.delete x) a (functions d) }

difference :: Instance -> Instance -> Record
difference old new
  | instanceSchema old /= instanceSchema new = Snapshot (definition (instanceSchema new)) (instanceData new)
  | otherwise = Changes (setChanges ++ functionChanges)
  where
    before = instanceData old; after = instanceData new
    setChanges = concat
      [ [Add o x | x <- S.toList (ys S.\\ xs)] ++ [Remove o x | x <- S.toList (xs S.\\ ys)]
      | (o,ys) <- M.toList (sets after), let xs = M.findWithDefault S.empty o (sets before) ]
    functionChanges = concat
      [ [Set a x y | (x,y) <- M.toList ys, M.lookup x xs /= Just y]
        ++ [Unset a x | x <- M.keys (xs `M.difference` ys)]
      | (a,ys) <- M.toList (functions after), let xs = M.findWithDefault M.empty a (functions before) ]

-- The callback is pure; the MVar serializes all callers on this handle.
-- Failed validation writes nothing. A failed disk commit has an uncertain
-- outcome and poisons the handle, forcing recovery before further use.
transaction :: Database -> (Instance -> Either Error (a,Instance)) -> IO (Either StoreError a)
transaction (Database cell) change = mask_ $ modifyMVar cell $ \state -> case state of
  Nothing -> pure (state,Left Closed)
  Just st | poisoned st -> pure (state,Left ReopenRequired)
  Just st -> case change (current st) of
    Left e -> pure (state,Left (DomainError e))
    Right (value,next)
      | next == current st -> pure (state,Right value)
      | otherwise -> do
          let bytes = frame (difference (current st) next)
          _ <- evaluate (B.length bytes)
          result <- capture (appendFrame (descriptor st) (endOffset st) bytes)
          case result of
            Left e -> pure (Just st { poisoned = True }, Left (CommitUncertain (T.pack (show e))))
            Right offset -> pure (Just st { current = next, endOffset = offset },Right value)

commit :: Database -> [Edit] -> IO (Either StoreError ())
commit db ops = transaction db (\i -> (,) () <$> edit ops i)

snapshot :: Database -> IO (Either StoreError Instance)
snapshot (Database cell) = withMVar cell $ \state -> pure $ case state of
  Nothing -> Left Closed
  Just st | poisoned st -> Left ReopenRequired
          | otherwise -> Right (current st)

recoveredBytes :: Database -> IO (Either StoreError Integer)
recoveredBytes (Database cell) = withMVar cell $ pure . maybe (Left Closed) (Right . toInteger . recovered)

closeDatabase :: Database -> IO (Either StoreError ())
closeDatabase (Database cell) = mask_ $ modifyMVar cell $ \state -> case state of
  Nothing -> pure (Nothing,Right ())
  Just st -> do
    result <- capture (closeHandle (descriptor st))
    pure (Nothing,result)

withDatabase :: FilePath -> (Database -> IO a) -> IO (Either StoreError a)
withDatabase path action = capture $
  bracket (openDatabase path >>= either failStore pure)
          (\db -> closeDatabase db >>= either failStore pure) action

-- A compact backup at a NEW path. The live database remains open and usable;
-- replace it only after closing. No unsafe in-place rewrite or sidecar file.
copyDatabase :: Database -> FilePath -> IO (Either StoreError ())
copyDatabase db path = do
  result <- snapshot db
  case result of
    Left e -> pure (Left e)
    Right i -> capture $ bracket (createDatabase path i >>= either failStore pure)
      (\copy -> closeDatabase copy >>= either failStore pure) (const (pure ()))

-- A compact, consistent native-file backup without creating a sidecar file.
backupBytes :: Database -> IO (Either StoreError B.ByteString)
backupBytes db = do
  result <- snapshot db
  pure $ do
    i <- result
    let bytes = frame (Snapshot (definition (instanceSchema i)) (instanceData i))
    if B.length bytes > maxPayload + 40
      then Left (FileError "compact backup exceeds the 256 MiB frame limit")
      else Right bytes
