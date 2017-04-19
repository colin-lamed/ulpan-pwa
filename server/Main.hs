import Application (makeApplication) -- for YesodDispatch instance

import BasicPrelude
import Data.Aeson.Types
import Data.Text
import Yesod.Default.Config (fromArgs, DefaultEnv)
import Yesod.Default.Main   (defaultMain)

data Extra = Extra
    { extraCopyright :: Maybe Text
    , extraAnalytics :: Maybe Text -- ^ Google Analytics
    } deriving Show

parseExtra :: DefaultEnv -> Object -> Parser Extra
parseExtra _ o = Extra
    <$> o .:? (pack "copyright")
    <*> o .:? (pack "analytics")

main :: IO ()
main = defaultMain (fromArgs parseExtra) makeApplication
