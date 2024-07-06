module MelbyClient.GitVersion
  ( gitVersion
  ) where

import Data.Time.LocalTime
import Language.Haskell.TH
import System.Environment
import System.Process

-- Adapted from
-- https://stackoverflow.com/questions/5713418/templatehaskell-and-io#comment7962237_5713551
-- and https://stackoverflow.com/a/20871010/437583.

gitVersion :: Q Exp
gitVersion = stringE =<< runIO getCombinedInfo

getCombinedInfo :: IO String
getCombinedInfo = do
  gi <- getGitInfo
  ti <- getTimeInfo
  pure $ concat [gi, "  (", ti, ")"]

getGitInfo :: IO String
getGitInfo = do
  -- We can't use the convenient "tGitInfoCwd" function from the GitHash package
  -- because it uses the current directory (and if we are built by cabal, we're
  -- not in the actual source directory but a copy of it in a sandboxed folder).
  -- The "tGitInfo" function is parameterized to take arbitrary paths, but it's
  -- difficult to customize its behavior. Because of this we just invoke git
  -- with our own flags, because it's pretty easy to do so.
  --
  -- Anyway, the point of MELBY_PROJECT_ROOT is so that we can set this
  -- environment variable when we invoke cabal, so that we can read it back out
  -- here. This way we can pass in environment variables to Template Haskell
  -- (compile-time code).
  maybeProjectRoot <- lookupEnv "MELBY_PROJECT_ROOT"
  case maybeProjectRoot of
    Just projectRoot -> readProcess "git"
      [ "-C"
      , projectRoot
      , "describe"
      , "--abbrev=10"
      , "--always"
      , "--dirty"
      ] ""
    Nothing -> pure "-unknown"

getTimeInfo :: IO String
getTimeInfo = show <$> getZonedTime
