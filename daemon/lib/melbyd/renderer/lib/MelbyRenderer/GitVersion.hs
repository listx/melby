module MelbyRenderer.GitVersion
  ( gitVersion
  ) where

import Data.Time.LocalTime
import Language.Haskell.TH
import System.Environment
import System.Process

gitVersion :: Q Exp
gitVersion = stringE =<< runIO getCombinedInfo

getCombinedInfo :: IO String
getCombinedInfo = do
  gi <- getGitInfo
  ti <- getTimeInfo
  pure $ concat [gi, "  (", ti, ")"]

getGitInfo :: IO String
getGitInfo = do
  maybeProjectRoot <- lookupEnv "MELBY_PROJECT_ROOT"
  case maybeProjectRoot of
    Just projectRoot -> readProcess "git"
      ["-C", projectRoot, "describe", "--abbrev=10", "--always", "--dirty"] ""
    Nothing -> pure "-unknown"

getTimeInfo :: IO String
getTimeInfo = show <$> getZonedTime
