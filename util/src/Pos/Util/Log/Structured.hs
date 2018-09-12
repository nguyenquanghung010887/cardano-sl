module Pos.Util.Log.Structured
       ( logMessageX
       , logDebugX
       , logInfoX
       , logNoticeX
       , logWarningX
       , logErrorX
       ) where

import           Universum

import           Pos.Util.Wlog.Compatibility (HasLoggerName (..), Severity (..),
                     logMX)

import qualified Katip.Core as KC

-- | Shortcut for 'logMessageX' to use according severity.
logDebugX, logInfoX, logNoticeX, logWarningX, logErrorX
    :: (HasLoggerName m, MonadIO m, KC.LogItem a)
    => a -> m ()
logDebugX   = logMessageX Debug
logInfoX    = logMessageX Info
logNoticeX  = logMessageX Notice
logWarningX = logMessageX Warning
logErrorX   = logMessageX Error

-- | Log an item in JSON format (only for JSON scribes).
logMessageX
    :: (HasLoggerName m, MonadIO m, KC.LogItem a)
    => Severity
    -> a
    -> m ()
logMessageX severity a = do
    name <- askLoggerName
    logMX name severity a
