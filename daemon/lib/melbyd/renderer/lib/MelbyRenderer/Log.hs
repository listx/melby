module MelbyRenderer.Log where

import Control.Monad.Logger
import Control.Monad.Logger.Prefix
import qualified Data.Text as T

logDebug :: MonadLogger m => T.Text -> m ()
logDebug t = prefixLogs "melbyr" $ logDebugN t
