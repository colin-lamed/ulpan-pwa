{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DeriveGeneric     #-}

module UlpanClient.VocabDrill
  ( initPage
  ) where

import BasicPrelude hiding (on, Ordering)
import Control.Concurrent.MVar (MVar, putMVar, takeMVar, newEmptyMVar)
import Data.Aeson (ToJSON(..), FromJSON(..), genericToEncoding, genericParseJSON, defaultOptions, decode)
import Data.Bifunctor (bimap)
import Data.Foldable (traverse_)
import Data.JSString (JSString, unpack)
import GHC.Generics (Generic)
import GHCJS.Types (JSVal)
import GHCJS.DOM (currentDocument)
import GHCJS.DOM.Types (Document, Element, fromJSString, toJSString)
import GHCJS.DOM.Document (getElementById)
import GHCJS.DOM.Element as E (click)
import GHCJS.DOM.EventM (on)
import System.Random (randomIO)
import System.Random.Shuffle (shuffleM)
import Text.Hamlet (shamlet)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Model.Vocab as Vocab
import Model.VocabSerialisation()
import UlpanClient.JSUtils as J
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BSL (fromStrict)
import qualified JavaScript.Web.XMLHttpRequest as Xhr

initPage :: JSVal -> IO ()
initPage jsInput = do

  initData <- J.fromJsVal jsInput :: IO (Maybe InitData)
  putStrLn $ "initPage: " <> show initData
  let Just (InitData languagesAvailable) = initData

  Just doc <- currentDocument

  enableSelectLanguageFileButton doc languagesAvailable
  enableChangeParametersButton   doc

  showTest doc


mkRequest :: JSString -> Xhr.Request
mkRequest uri = Xhr.Request
  { Xhr.reqMethod          = Xhr.GET
  , Xhr.reqURI             = fromJSString uri
  , Xhr.reqLogin           = Nothing
  , Xhr.reqHeaders         = []
  , Xhr.reqWithCredentials = False
  , Xhr.reqData            = Xhr.NoData
}

loadResource :: (MonadIO m, FromJSON a) => JSString -> m (Maybe a)
loadResource url = do
  response <- liftIO $ Xhr.xhrByteString $ mkRequest url
  return $ case Xhr.status response of
    200 -> Xhr.contents response >>= decode . BSL.fromStrict
    _   -> Nothing


showTest :: Document -> IO ()
showTest doc = do
  setStatus doc "Loading"
  mLanguageFile <- getLocalStorage "currentFile"
  case mLanguageFile of
    Just languageFile -> startTest doc languageFile
    Nothing          -> do
                        clearStatus doc
                        setContent doc "Select a file from menu to start."


enableSelectLanguageFileButton :: Document -> [(Text, Text)] -> IO ()
enableSelectLanguageFileButton doc availableLanguages = do
  selectLanguageBtn <- byId doc "select-language"
  on selectLanguageBtn E.click $ do
    clearStatus doc
    currentFile <- getLocalStorage "currentFileId" >>= return . fromMaybe "" :: MonadIO m => m String
    -- mOptions    <- loadResource "/languagefilesNames" :: MonadIO m => m (Maybe [(String,String)])
    -- case mOptions of
    --   Just options ->
    let options = bimap T.unpack T.unpack <$> availableLanguages
    setContent doc $ renderHtml [shamlet|
                        <div class="form">
                          <div class="form-group">
                            <label for="language-file">Select Language File
                            <select id="language-file" class="form-control">
                              $forall (v, l) <- options
                                <option value=#{v} :currentFile == v:selected>#{l}
                        <button id="apply" class="btn btn-default">Apply
                      |]
      -- Nothing      -> setStatus doc "Could not load language files list"

    applyBtn <- liftIO $ byId doc "apply"
    liftIO $ on applyBtn E.click $ do
      languageFileId <- liftIO $ jsGetSelection "language-file"
      setStatus doc "Downloading data file..."
      clearContent doc
      mLanguageFile <- loadResource $ "/languagefile/" <> languageFileId
      case mLanguageFile of
        Just languageFile -> liftIO $ do
                             setLocalStorage "currentFile" languageFile
                             setLocalStorage "currentFileId" (fromJSString languageFileId :: Text)
                             startTest doc languageFile
        Nothing           -> setStatus doc "Failed to read downloaded data file"
    liftIO hideMenu
  return ()

enableChangeParametersButton :: Document -> IO ()
enableChangeParametersButton doc = do
  changeParamsBtn <- byId doc "change-parameters"
  testParams      <- getTestParams
  let orderings  = (id &&& show) <$> [minBound..maxBound]
      directions = (id &&& show) <$> [minBound..maxBound]
  on changeParamsBtn E.click $ do
    clearStatus doc
    setContent doc $ renderHtml [shamlet|
      <div class="form">
        Change Parameters
        <br>
        <div class="form-group">
          <label for="param-ordering">Ordering
          <select id="param-ordering" class="form-control">
            $forall (v, l) <- orderings
              <option value=#{show v} :ordering testParams == v:selected>#{l}
        <div class="form-group">
          <label for="param-test-direction">Test Direction
          <select id="param-test-direction" class="form-control">
            $forall (v, l) <- directions
              <option value=#{show v} :testDirection testParams == v:selected>#{l}
        <div class="form-group">
          <label for="show-notes">Show Notes
          <input type="checkbox" id="show-notes" class="form-control" :showNotes testParams:checked>
        <button id="apply" class="btn btn-default">Apply
        <button id="cancel" class="btn btn-default">Cancel
    |]

    applyBtn <- byId doc "apply"
    liftIO $ on applyBtn E.click $ do
      ordering      <- liftIO $ jsGetSelection "param-ordering" >>= return . read . T.pack . unpack
      testDirection <- liftIO $ jsGetSelection "param-test-direction" >>= return . read . T.pack . unpack
      showNotes     <- liftIO $ jsIsChecked "show-notes"
      setLocalStorage "testParams" (TestParams ordering testDirection showNotes)
      liftIO $ showTest doc
    cancelBtn <- byId doc "cancel"
    liftIO $ on cancelBtn E.click $ do
      liftIO $ showTest doc
    liftIO hideMenu
  return ()

startTest :: Document -> LanguageFile -> IO ()
startTest doc languageFile = do
  testParams <- getTestParams
  clearStatus doc
  setContent doc $ renderHtml [shamlet|
    <h3 align="center">
      <span id="title_span">
    <table class="test-table">
      <tr>
        <td id="question_td" class="textarea">
          <div id="question">
            <span id="question_span">
      <tr>
        <td style="height: 25px">&nbsp;
      <tr>
        <td id="answer_td" class="textarea">
          <div id="answer">
            <span id="answer_span">
      <tr>
        <td style="height: 25px">&nbsp;
      $if showNotes testParams
        <tr>
          <td class="textarea">
            <div id="notes">
      <tr>
        <td>&nbsp;
      <tr>
        <td align="center">
          <div class="btn-group">
            <button id="show" type="submit" class="btn btn-default">
              <span class="glyphicon glyphicon-eye-open">
              Show
          <div class="btn-group">
            <button id="next" type="submit" class="btn btn-success">
              <span class="glyphicon glyphicon-circle-arrow-right">
              Next
  |]

  index <- mkIdx (ordering testParams) (length $ lfVocab languageFile)
  initQuestion doc languageFile index

nextEntry :: LanguageFile -> TestParams -> Index -> IO (VocabEntry, Index)
nextEntry languageFile testParams index = do
  -- TODO filter by selected Groups - requires exposing in test params selection
-- getIterable :: Vocab -> Ordering -> [Group] -> StdGen -> [VocabEntry]
-- getIterable (Vocab _ vocabMap) ordering groups shuffleSeed =
--   let vocabToTest = concat $ M.elems $ M.filterWithKey (\k _ -> k `elem` groups) vocabMap
-- allGroups :: LanguageFile -> [Group]
-- allGroups = M.keys . lfVocab
  let vocab = join $ M.elems $ lfVocab languageFile
  (i, index') <- getNextIndex index
  return (vocab !! i, index')

initQuestion :: Document -> LanguageFile -> Index -> IO ()
initQuestion doc languageFile index = do
  testParams <- getTestParams
  (VocabEntry l1 l2 note, index') <- nextEntry languageFile testParams index
  let meta = lfMeta languageFile

  fromL1 <- case testDirection testParams of
        FromLanguage1 -> return True
        FromLanguage2 -> return False
        DShuffled     -> randomIO

  jsChangeTableColour "question" "white"
  jsHide "answer"
  jsChangeTableColour "answer" "lightgrey"

  traverse_ (removeClass "question_td" . toJSString . show) [GCentre, GRight, GLeft]
  traverse_ (removeClass "answer_td"   . toJSString . show) [GCentre, GRight, GLeft]
  traverse_ (removeClass "question_td" . toJSString . show) [Small, Medium, Large, XLarge]
  traverse_ (removeClass "answer_td"   . toJSString . show) [Small, Medium, Large, XLarge]

  if fromL1
    then do
      jsSetSpan "question_span" (toJSString $ lText l1)
      jsSetSpan "answer_span"   (toJSString $ lText l2)
      addClass "question_td" (toJSString $ show $ language1Gravity  meta)
      addClass "question_td" (toJSString $ show $ language1TextSize meta)
      addClass "answer_td"   (toJSString $ show $ language2Gravity  meta)
      addClass "answer_td"   (toJSString $ show $ language2TextSize meta)
    else do
      jsSetSpan "question_span" (toJSString $ lText l2)
      jsSetSpan "answer_span"   (toJSString $ lText l1)
      addClass "question_td" (toJSString $ show $ language2Gravity  meta)
      addClass "question_td" (toJSString $ show $ language2TextSize meta)
      addClass "answer_td"   (toJSString $ show $ language1Gravity  meta)
      addClass "answer_td"   (toJSString $ show $ language1TextSize meta)


  jsSetSpan "title_span" $ toJSString $ name meta <> " (" <> show (ordering testParams) <> ")"

  when (showNotes testParams) $ do
    jsSetSpan "notes" $ toJSString note
    jsChangeTableColour "notes" ""
    jsHide "notes"

  jsHide "next"
  jsShow "show"

  nextBtn <- byId doc "next"
  on nextBtn E.click $ liftIO $ initQuestion doc languageFile index'
  showBtn <- byId doc "show"
  on showBtn E.click $ liftIO showAnswer
  return ()

showAnswer :: IO ()
showAnswer = do
  jsShow "answer"
  jsChangeTableColour "answer" "white"
  testParams <- getTestParams
  when (showNotes testParams) $ do
    jsShow "notes"
    jsChangeTableColour "notes" "white"
  jsShow "next"
  jsHide "show"



data Ordering = Random
              | Shuffled
              | FileOrdered
  deriving (Read, Show, Eq, Enum, Bounded, Generic)

data TestDirection = FromLanguage1
                   | FromLanguage2
                   | DShuffled
  deriving (Read, Show, Eq, Enum, Bounded, Generic)



data TestParams = TestParams
  { ordering      :: Ordering
  , testDirection :: TestDirection
  , showNotes     :: Bool
  } deriving (Show, Generic)

instance ToJSON Ordering where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Ordering where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON TestDirection where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TestDirection where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON TestParams where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TestParams where
  parseJSON = genericParseJSON defaultOptions

getTestParams :: IO TestParams
getTestParams = do
  mTestParams <- getLocalStorage "testParams"
  return $ case mTestParams of
    Just testParams -> testParams
    Nothing         -> TestParams FileOrdered FromLanguage1 False



data Index = FileOrderedIndex { iLength :: Int, iCurrent :: Int }
           | ShuffledIndex { iLength :: Int, iShuffledIndices :: [Int] }

mkIdx :: Ordering -> Int -> IO Index
mkIdx FileOrdered length =
  return $ FileOrderedIndex length 0
mkIdx Shuffled length =
  shuffleM [0..length] >>= return . ShuffledIndex 0

getNextIndex :: Index -> IO (Int, Index)
getNextIndex (FileOrderedIndex length current) = do
  let next = if current >= length - 1
               then 0
               else current + 1
  return (next, FileOrderedIndex length next)
getNextIndex (ShuffledIndex length shuffledIndices) = do
  case shuffledIndices of
    next : shuffledIndices -> return (next, ShuffledIndex length shuffledIndices)
    []                     -> mkIdx Shuffled length >>= getNextIndex
