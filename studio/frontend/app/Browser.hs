{-# LANGUAGE CPP, JavaScriptFFI #-}
module Browser (sessionToken, markDirty, download) where
import Miso.String (MisoString)
#ifdef WASM
import Miso.DSL (JSVal, fromJSValUnchecked, toJSVal)
foreign import javascript unsafe "window.catdbBrowser.token()" tokenJS :: IO JSVal
foreign import javascript unsafe "window.catdbBrowser.setDirty($1)" dirtyJS :: Bool -> IO ()
foreign import javascript safe "window.catdbBrowser.download($1).then(() => '').catch(e => String(e.message))" downloadJS :: JSVal -> IO JSVal
sessionToken :: IO MisoString
sessionToken = fromJSValUnchecked =<< tokenJS
markDirty :: Bool -> IO ()
markDirty = dirtyJS
download :: MisoString -> IO MisoString
download kind = fromJSValUnchecked =<< downloadJS =<< toJSVal kind
#else
sessionToken :: IO MisoString
sessionToken = pure ""
markDirty :: Bool -> IO ()
markDirty _ = pure ()
download :: MisoString -> IO MisoString
download _ = pure "Downloads require the browser."
#endif
