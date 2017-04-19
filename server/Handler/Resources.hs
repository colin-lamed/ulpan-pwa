{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handler.Resources
  ( staticResourcesExp
  , randStringD
  , randStringExp
  ) where

import BasicPrelude
import Data.Maybe (fromJust)
import Language.Haskell.TH (clause, funD, listE, litE, mkName, normalB, stringL, runIO, DecsQ, Q, Exp(..))
import System.Directory (getDirectoryContents, doesFileExist)
import System.Random (randomRs, newStdGen)
import Yesod.Static (staticFiles)

listSubfiles :: String -> IO [String]
listSubfiles path = do
  let isFile entry = doesFileExist (path </> entry)
      filterHidden paths = return $ filter (\path -> head path /= '.') paths
  all <- getDirectoryContents path >>= filterHidden
  join <$> (flip traverse) all (\a -> do
              isFile' <- isFile a
              if isFile'
                then return [path </> a]
                else listSubfiles (path </> a))

staticResources :: String -> IO [String]
staticResources path = do
  (fromJust . stripPrefix path <$>) <$> listSubfiles path

staticResourcesExp :: String -> Q Exp
staticResourcesExp path = do
  runIO (staticResources path) >>= listE . map (litE . stringL)


--------------------------------------------------------------------------------

-- A standard function generating random strings.
randString :: IO String
randString = liftM (take 10 . randomRs ('a','z')) newStdGen

-- .. lifted to an Q Exp
randStringExp :: Q Exp
randStringExp = runIO randString >>= litE . stringL

-- | Declares a constant `String` function with a given name
-- that returns a random string generated on compile time.
randStringD :: String -> DecsQ
randStringD fname = liftM (: []) $
    funD (mkName fname) [clause [] (normalB randStringExp) []]
