module UlpanClient.JSUtils where

import BasicPrelude
import Control.Concurrent.MVar (MVar, putMVar, takeMVar, newEmptyMVar)
import Data.Aeson (ToJSON(..), FromJSON(..), encode, decode)
import Data.List.Extra (splitOn)
import Data.Maybe (fromJust)
import Data.JSString (JSString, unpack, pack)
import GHCJS.DOM (currentDocument)
import GHCJS.DOM.Document (getElementById)
import GHCJS.DOM.Element as E (click, setInnerHTML)
import GHCJS.DOM.Types (Document, Element, ToJSString, toJSString, FromJSString,
  fromJSString, fromMaybeJSString)
import GHCJS.Nullable (Nullable)
import GHCJS.Types (JSVal)
import qualified Data.ByteString.Lazy as BSL (fromStrict, toStrict)
import qualified Data.Text.Lazy as LT (Text, toStrict)


foreign import javascript unsafe
  "document.getElementById($1).style.display='';"
  jsShow :: JSString -> IO ()

foreign import javascript unsafe
  "document.getElementById($1).style.display='none';"
  jsHide :: JSString -> IO ()

foreign import javascript unsafe
  "document.getElementById($1).parentNode.style.background=$2;"
  jsChangeTableColour :: JSString -> JSString -> IO ()

foreign import javascript unsafe
  "document.getElementById($1).textContent=$2;"
  jsSetSpan :: JSString -> JSString -> IO ()

setSpan :: (ToJSString a, ToJSString b) => a -> b -> IO ()
setSpan a b = jsSetSpan (toJSString a) (toJSString b)

foreign import javascript unsafe
  "$r = document.getElementById($1).className;"
  jsGetClass :: JSString -> IO JSString

foreign import javascript unsafe
  "document.getElementById($1).className=$2;"
  jsSetClass :: JSString -> JSString -> IO ()

foreign import javascript unsafe
  " var elt = document.getElementById($1); \
  \ var selection = null;                  \
  \ if (elt.selectedIndex != -1)           \
  \   selection = elt.options[elt.selectedIndex].value;\
  \ $r = selection;"
  jsGetSelection :: JSString -> IO JSString

foreign import javascript unsafe
  "$r = document.getElementById($1).checked;"
  jsIsChecked :: JSString -> IO Bool


foreign import javascript unsafe
  "localStorage.setItem($1, $2);"
  jsSetLocalStorage :: JSString -> JSString -> IO ()

foreign import javascript unsafe
  "$r = localStorage.getItem($1);"
  jsFromLocalStorage :: JSString -> IO (Nullable JSString)

addClass :: JSString -> JSString -> IO ()
addClass elmt clazz = do
  clazzes <- jsGetClass elmt
  jsSetClass elmt $ clazzes <> " " <> clazz

removeClass :: JSString -> JSString -> IO ()
removeClass elmt clazz =
  jsGetClass elmt >>=
      jsSetClass elmt
    . pack
    . intercalate " "
    . filter (/= (fromJSString clazz))
    . splitOn " "
    . unpack


foreign import javascript unsafe
  "document.querySelector('.menu__overlay').dispatchEvent(new MouseEvent('click', {\
  \  'view': window,     \
  \  'bubbles': true,    \
  \  'cancelable': false \
  \}));"
  hideMenu :: IO ()

setLocalStorage :: (MonadIO m, ToJSON a) => JSString -> a -> m ()
setLocalStorage key val = do
  let bs = toJSString $ decodeUtf8 $ BSL.toStrict $ encode val
  liftIO $ jsSetLocalStorage key bs

getLocalStorage :: (MonadIO m, FromJSON a) => JSString -> m (Maybe a)
getLocalStorage key = do
  a <- liftIO $ fromMaybeJSString <$> jsFromLocalStorage key
  return $ a >>= decode . BSL.fromStrict . encodeUtf8

byId :: MonadIO m => Document -> JSString -> m Element
byId doc id = fromJust <$> getElementById doc id

setStatus :: MonadIO m => Document -> JSString -> m ()
setStatus doc txt = do
  statusDiv <- byId doc "status"
  E.setInnerHTML statusDiv $ Just txt

clearStatus :: MonadIO m => Document -> m ()
clearStatus doc = do
  statusDiv <- byId doc "status"
  E.setInnerHTML statusDiv (Nothing :: Maybe Text)

setContent :: MonadIO m => Document -> LT.Text -> m ()
setContent doc txt = do
  contentDiv <- byId doc "content"
  E.setInnerHTML contentDiv $ Just $ LT.toStrict txt

clearContent :: MonadIO m => Document -> m ()
clearContent doc = do
  contentDiv <- byId doc "content"
  E.setInnerHTML contentDiv (Nothing :: Maybe Text)

foreign import javascript unsafe
  "$r = JSON.stringify($1);"
  stringify :: JSVal -> IO JSString

fromJsVal :: FromJSON a => JSVal -> IO (Maybe a)
fromJsVal a = do
  s <- stringify a
  return $ decode $ BSL.fromStrict $ encodeUtf8 $ fromJSString s
