-- Copyright 2024 Linus Arver
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

module PostTangleUtil.GitVersion
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
      [ "-C"
      , projectRoot
      , "describe"
      , "--abbrev=10"
      , "--always"
      , "--dirty"
      ]
      ""
    Nothing -> pure "-unknown"

getTimeInfo :: IO String
getTimeInfo = show <$> getZonedTime
