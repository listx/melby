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

module MelbyRenderer.Log where

import Control.Monad.Logger
import Control.Monad.Logger.Prefix
import qualified Data.Text as T

-- FIXME: Make this debug output an environment variable option (only turn it on
-- if lookupEnv says MELBYR_VERBOSE is true).
logDebug :: MonadLogger m => T.Text -> m ()
logDebug t = prefixLogs "melbyr" $ logDebugN t
