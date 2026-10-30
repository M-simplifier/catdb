{-# LANGUAGE DataKinds, TypeOperators, OverloadedStrings #-}
module Studio.Server (application) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, encode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import Data.IORef
import Network.HTTP.Types hiding (Header)
import Network.Wai hiding (Request)
import qualified Network.Wai as Wai
import Servant hiding (respond)
import Studio.Core (Request, Failure, problem)
import Studio.Runtime
import Studio.Wire

type API = "api" :> "state" :> Get '[JSON] WState
      :<|> "api" :> "command" :> ReqBody '[JSON] Request :> Post '[JSON] WReply
      :<|> "api" :> "export" :> Get '[JSON] (Headers '[Header "Content-Disposition" String] Value)
      :<|> "api" :> "backup" :> Get '[OctetStream] (Headers '[Header "Content-Disposition" String] L.ByteString)
      :<|> Raw

application :: Int -> B.ByteString -> FilePath -> Runtime -> Application
application port token assets runtime = secure port token $ indexRoute $ serve (Proxy :: Proxy API)
  (liftIO (readState runtime) :<|> (liftIO . submit runtime) :<|> export :<|> backup :<|> serveDirectoryWebApp assets)
  where
    export = do
      result <- liftIO (definitionExport runtime)
      either reject (pure . addHeader "attachment; filename=catdb-definitions.json") result
    backup = do
      result <- liftIO (nativeBackup runtime)
      either reject (pure . addHeader "attachment; filename=catdb-backup.catdb" . L.fromStrict) result
    reject :: Failure -> Handler a
    reject e = throwError err400 {errBody=encode (problem e),errHeaders=[(hContentType,"application/json")]}

indexRoute :: Middleware
indexRoute next request respond = next routed respond
  where routed = if null (pathInfo request) then request {pathInfo=["index.html"],rawPathInfo="/index.html"} else request

secure :: Int -> B.ByteString -> Middleware
secure port token next request respond = do
  let authority = ":" <> BC.pack (show port)
      hosts = ["127.0.0.1" <> authority,"localhost" <> authority]
      origins = map ("http://" <>) hosts
      hostOK = maybe False (`elem` hosts) (requestHeaderHost request)
      originOK = maybe True (`elem` origins) (lookup hOrigin (requestHeaders request))
      siteOK = lookup "Sec-Fetch-Site" (requestHeaders request) /= Just "cross-site"
      api = case pathInfo request of "api":_ -> True; _ -> False
      authenticated = lookup "X-CatDB-Session" (requestHeaders request) == Just token
      mutation = requestMethod request /= methodGet && requestMethod request /= methodHead
      reject status message = respond (responseLBS status ((hContentType,"application/json"):securityHeaders)
        (encode (WReply False message (Just (WProblem "LocalSessionRequired" message "")) Nothing Nothing Nothing)))
      response = respond . mapResponseHeaders (securityHeaders ++)
  if not (hostOK && originOK && siteOK) then reject status403 "この端末のCatDB Studioから操作してください。"
    else if api && not authenticated then reject status403 "起動時に表示されたURLからCatDB Studioを開いてください。"
    else if mutation then do
      body <- readBoundedBody request (4*1024*1024)
      case body of
        Nothing -> reject status413 "入力が4 MiBを超えています。処理する範囲を小さくしてください。"
        Just bytes -> do
          remaining <- newIORef bytes
          let reader = atomicModifyIORef' remaining (\value -> (B.empty,value))
          next request {requestBody=reader} response
    else next request response

securityHeaders :: ResponseHeaders
securityHeaders =
  [(hCacheControl,"no-store"),("X-Content-Type-Options","nosniff"),("Referrer-Policy","no-referrer"),("X-Frame-Options","DENY")
  ,("Content-Security-Policy","default-src 'self'; script-src 'self' 'wasm-unsafe-eval'; style-src 'self' 'unsafe-inline'; connect-src 'self'; img-src 'self' data:; frame-ancestors 'none'; base-uri 'none'; form-action 'self'")]

readBoundedBody :: Wai.Request -> Int -> IO (Maybe B.ByteString)
readBoundedBody request maximumSize = loop [] 0
  where
    loop chunks size = do
      chunk <- getRequestBodyChunk request
      let total = size+B.length chunk
      if total>maximumSize then pure Nothing
        else if B.null chunk then pure (Just (B.concat (reverse chunks))) else loop (chunk:chunks) total
