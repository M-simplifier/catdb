{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import CatDB
import Relay.Model
import Control.Exception (bracket)
import Data.Aeson (encode, eitherDecode, object, (.=))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import System.Directory (doesFileExist, makeAbsolute)
import System.Environment (getArgs, getExecutablePath)
import System.Exit (die)
import System.FilePath ((</>), takeDirectory, takeFileName)
import System.IO (hSetEncoding, hSetBuffering, BufferMode(LineBuffering), stdout, stderr, utf8)

data Options = Options { dbPath :: FilePath, port :: Int, assets :: Maybe FilePath, seedDemo :: Bool }

options :: [String] -> Either String Options
options (file:args) = parse (Options file 4283 Nothing False) args
  where
    parse opts [] = Right opts
    parse opts ("--demo":rest) = parse opts {seedDemo=True} rest
    parse opts ("--assets":folder:rest) = parse opts {assets=Just folder} rest
    parse opts ("--port":number:rest) = case reads number of
      [(p,"")] | p >= 1024 && p <= 65535 -> parse opts {port=p} rest
      _ -> Left "Port must be between 1024 and 65535."
    parse _ _ = Left usage
options [] = Left usage

usage :: String
usage = "relay-desk FILE.catdb [--demo] [--port 4283] [--assets reference/web]\nThe server binds to 127.0.0.1 only. --demo seeds only a newly created file."

must :: Show e => Either e a -> IO a
must = either (die . show) pure

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  hSetBuffering stdout LineBuffering
  opts <- getArgs >>= either die pure . options
  file <- makeAbsolute (dbPath opts)
  exists <- doesFileExist file
  initial <- must (if seedDemo opts then demoWorkspace else workspace)
  exe <- getExecutablePath
  let candidates = maybe [] (:[]) (assets opts) ++ [takeDirectory exe </> "relay-web","reference/web"]
  webRoot <- findAssets candidates
  bracket ((if exists then openDatabase file else createDatabase file initial) >>= must)
          (\db -> closeDatabase db >>= must) $ \db -> do
    actual <- snapshot db >>= must
    expected <- must workspace
    if instanceSchema actual /= instanceSchema expected then die "This file is not a Relay Desk workspace." else pure ()
    putStrLn ("Relay Desk / CatDB PoC\nDatabase: " ++ file ++ "\nOpen http://127.0.0.1:" ++ show (port opts) ++ "\nPress Ctrl+C to stop.")
    runSettings (setPort (port opts) (setHost "127.0.0.1" defaultSettings)) (application db file webRoot (port opts))

findAssets :: [FilePath] -> IO FilePath
findAssets [] = die "Cannot find relay-web/index.html. Run from the release folder, or pass --assets reference/web."
findAssets (folder:rest) = do
  exists <- doesFileExist (folder </> "index.html")
  if exists then pure folder else findAssets rest

headers :: ResponseHeaders
headers = [(hCacheControl,"no-store"),("X-Content-Type-Options","nosniff"),("X-Frame-Options","DENY"),
  ("Content-Security-Policy","default-src 'self'; script-src 'self'; style-src 'self'; connect-src 'self'; img-src 'self' data:; frame-ancestors 'none'; base-uri 'none'; form-action 'self'")]

application :: Database -> FilePath -> FilePath -> Int -> Application
application db file webRoot p request respond
  | requestHeaderHost request `notElem` map Just allowedHosts = json status403 (failure "Unexpected Host header.")
  | otherwise = case (requestMethod request,pathInfo request) of
      ("GET",[]) -> static "index.html" "text/html; charset=utf-8"
      ("GET",["app.js"]) -> static "app.js" "text/javascript; charset=utf-8"
      ("GET",["style.css"]) -> static "style.css" "text/css; charset=utf-8"
      ("GET",["api","state"]) -> withSnapshot (\i -> either (json status400 . failure . errorMessage) (json status200 . wrap) (dashboard i))
      ("GET",["api","public"]) -> withSnapshot (\i -> either (json status400 . failure . errorMessage) (json status200) (publicExport i))
      ("GET",["api","backup"]) -> do
        result <- backupBytes db
        case result of
          Left e -> json status500 (failure (T.pack (show e)))
          Right bytes -> respond (responseLBS status200 ((hContentType,"application/octet-stream"):
            ("Content-Disposition","attachment; filename=relay-backup.catdb"):headers) (L.fromStrict bytes))
      ("POST",["api","command"])
        | lookup "X-CatDB-Local" (requestHeaders request) /= Just "1"
          || maybe False (`notElem` allowedOrigins) (lookup "Origin" (requestHeaders request))
          -> json status403 (failure "Use the local Relay Desk page to change this database.")
        | otherwise -> do
          body <- boundedBody request
          case body >>= either (Left . T.pack) Right . eitherDecode of
            Left e -> json status400 (failure e)
            Right command -> do
              result <- transaction db $ \i -> do
                next <- applyCommand command i
                display <- dashboard next
                pure (display,next)
              case result of
                Left (DomainError e) -> json status400 (failure (errorMessage e))
                Left e -> json status500 (failure (T.pack (show e)))
                Right display -> json status200 (wrap display)
      _ -> json status404 (failure "Not found.")
  where
    allowedHosts = map (<> BC.pack (":" ++ show p)) ["127.0.0.1","localhost"]
    allowedOrigins = map ("http://" <>) allowedHosts
    json status value = respond (responseLBS status ((hContentType,"application/json; charset=utf-8"):headers) (encode value))
    failure message = object ["error" .= (message :: Text)]
    wrap value = object ["workspace" .= value,"database" .= takeFileName file]
    static path contentType = respond (responseFile status200 ((hContentType,contentType):headers) (webRoot </> path) Nothing)
    withSnapshot action = snapshot db >>= either (json status500 . failure . T.pack . show) action

boundedBody :: Request -> IO (Either Text L.ByteString)
boundedBody request = go 0 []
  where
    go count chunks = do
      chunk <- getRequestBodyChunk request
      let total = count + B.length chunk
      if total > 5*1024*1024 then pure (Left "Import is larger than 5 MiB.")
      else if B.null chunk then pure (Right (L.fromChunks (reverse chunks))) else go total (chunk:chunks)
