module UserPrefs where

import BotEnv (UserPrefs)
import Config (Config)
import qualified Config as C
import Control.Monad.Reader (ReaderT, asks, liftIO)
import Data.HashMap.Lazy (fromList)
import Data.Maybe (catMaybes)
import System.Directory (createDirectoryIfMissing, listDirectory)
import System.FilePath ((</>), takeFileName)
import Text.Read (readMaybe)

cacheUserPrefs :: ReaderT Config IO UserPrefs
cacheUserPrefs = do
  prefsDir <- asks C.userPrefsDir
  liftIO $ createDirectoryIfMissing True prefsDir
  files <- liftIO $ listDirectory prefsDir
  prefs <- liftIO $ catMaybes <$> mapM (readUserPref . (prefsDir </>)) files
  return $ fromList prefs

readUserPref :: FilePath -> IO (Maybe (Int, Int))
readUserPref file = do
  content <- readFile file
  return $ do
    userId <- readMaybe . takeFileName $ file
    count <- readMaybe content
    return (userId, count)
