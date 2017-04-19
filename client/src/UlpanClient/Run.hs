{-# LANGUAGE RecordWildCards   #-}

module UlpanClient.Run where

import BasicPrelude
import Data.JSString (JSString)
import GHCJS.Foreign.Callback (Callback, syncCallback, syncCallback1, syncCallback2,
  syncCallback3, syncCallback2', OnBlocked(ContinueAsync))
import UlpanClient.VocabDrill (initPage)

foreign import javascript unsafe
  "js_init = $1"
  register_init :: Callback a -> IO ()


foreign import javascript unsafe
  "if ('serviceWorker' in navigator) { \
    \navigator.serviceWorker\
      \.register('./service-worker.js')\
      \.then(function() { console.log('Service Worker Registered'); }); \
  \}"
  register_service_worker :: IO ()


foreign import javascript unsafe
  "var event = new CustomEvent($1, {\
  \  'view': window,                \
  \  'bubbles': true,               \
  \  'cancelable': false            \
  \});                              \
  \window.dispatchEvent(event);"
  jsDispatchEvent :: JSString -> IO ()

run :: IO ()
run = do
  register_service_worker

  putStrLn "registering methods"
  register_init  =<< syncCallback1 ContinueAsync initPage
  -- let anyone know they can call js_init now
  jsDispatchEvent "js_init_ready"
