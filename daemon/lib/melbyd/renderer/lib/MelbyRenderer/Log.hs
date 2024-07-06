module MelbyRenderer.Log where

import Control.Monad.Logger
import Control.Monad.Logger.Prefix
import qualified Data.Text as T

-- FIXME: Make this debug output an environment variable option (only turn it on
-- if lookupEnv says MELBYR_VERBOSE is true).
logDebug :: MonadLogger m => T.Text -> m ()
logDebug t = prefixLogs "melbyr" $ logDebugN t
