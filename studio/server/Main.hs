{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}
module Main where

import Control.Exception (bracket, finally)
import Control.Monad (unless, when, void)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Foreign (Ptr, allocaBytes, castPtr)
import Foreign.C (CInt(..))
import Network.Socket hiding (shutdown)
import Network.Wai.Handler.Warp
import Numeric (showHex)
import Studio.Core (Request(OpenFile))
import Studio.Runtime
import Studio.Server
import Studio.Wire (replyOk,replyMessage)
import qualified Data.Text.IO as T
import System.Directory (doesFileExist)
import System.Environment (getArgs, getExecutablePath)
import System.Exit (die)
import System.FilePath ((</>),takeDirectory)
import System.Info (os)
import System.IO (hSetEncoding,hSetBuffering,BufferMode(LineBuffering),stdout,stderr,utf8)
import System.Process (createProcess,proc)

foreign import ccall safe "studio_random" randomBytes :: Ptr () -> CInt -> IO CInt

data Options = Options { optionPort :: Int, optionAssets :: Maybe FilePath, optionData :: Maybe FilePath, optionBrowser :: Bool, optionFile :: Maybe FilePath }
options :: [String] -> Either String Options
options = go (Options 0 Nothing Nothing False Nothing)
  where
    go o [] = Right o
    go o ("--port":n:rest) = case reads n of
      [(p,"")] | p==0 || p>=1024 && p<=65535 -> go o {optionPort=p} rest
      _ -> Left "Port must be 0 or between 1024 and 65535."
    go o ("--assets":p:rest) = go o {optionAssets=Just p} rest
    go o ("--data-dir":p:rest) = go o {optionData=Just p} rest
    go o ("--file":p:rest) = go o {optionFile=Just p} rest
    go o ("--browser":rest) = go o {optionBrowser=True} rest
    go _ _ = Left "catdb-studio [--browser] [--file FILE.catdb] [--port 0] [--assets studio/web] [--data-dir DIRECTORY]"

main :: IO ()
main = withSocketsDo $ do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  hSetBuffering stdout LineBuffering
  opts <- getArgs >>= either die pure . options
  exe <- getExecutablePath
  assets <- findAssets (maybe [] (:[]) (optionAssets opts) ++ [takeDirectory exe </> "studio-web","studio/web"])
  let dataRoot = maybe (takeDirectory exe </> "data") id (optionData opts)
  runtime <- newRuntime dataRoot
  case optionFile opts of
    Nothing -> pure ()
    Just file -> do
      result <- submit runtime (OpenFile file)
      unless (replyOk result) (T.putStrLn (replyMessage result) >> die "Could not open the requested file.")
  token <- allocaBytes 32 $ \ptr -> do
    ok <- randomBytes (castPtr ptr) 32
    unless (ok==1) (die "Could not create a secure local session.")
    bytes <- B.packCStringLen (castPtr ptr,32)
    pure (BC.pack (concatMap (\x -> let hex = showHex x "" in if length hex==1 then '0':hex else hex) (B.unpack bytes)))
  bracket (socket AF_INET Stream defaultProtocol) close $ \sock -> do
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet (fromIntegral (optionPort opts)) (tupleToHostAddress (127,0,0,1)))
    listen sock 64
    addr <- getSocketName sock
    p <- case addr of SockAddrInet number _ -> pure (fromIntegral number); _ -> die "Expected a loopback socket."
    let url = "http://127.0.0.1:" ++ show p ++ "/#" ++ BC.unpack token
        ready = do
          putStrLn ("CatDB Studio v0.4.0-poc\nOpen " ++ url ++ "\nData directory: " ++ dataRoot ++ "\nPress Ctrl+C or close this window to stop.")
          when (optionBrowser opts) (openBrowser url)
        settings = setBeforeMainLoop ready (setPort p (setHost "127.0.0.1" (setServerName "CatDB Studio" defaultSettings)))
    runSettingsSocket settings sock (application p token assets runtime) `finally` shutdown runtime

openBrowser :: String -> IO ()
openBrowser url = void (createProcess (proc command args))
  where
    (command,args) = if os=="mingw32" then ("rundll32.exe",["url.dll,FileProtocolHandler",url])
      else if os=="darwin" then ("open",[url]) else ("xdg-open",[url])

findAssets :: [FilePath] -> IO FilePath
findAssets [] = die "Cannot find the compiled Studio frontend. Keep studio-web beside catdb-studio, or pass --assets studio/web."
findAssets (p:rest) = do
  exists <- doesFileExist (p </> "studio-gui.wasm")
  if exists then pure p else findAssets rest
