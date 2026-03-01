module Coop.App.Log
  ( withLogEnv
  , logInfo
  , logWarn
  , logError
  , logDebug
  , parseLogLevel
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Katip
import System.IO (stdout)

withLogEnv :: Severity -> (LogEnv -> IO a) -> IO a
withLogEnv minLevel action = do
  le <- initLogEnv "coop" "production"
  scribe <- mkHandleScribe ColorIfTerminal stdout (permitItem minLevel) V2
  le' <- registerScribe "stdout" scribe defaultScribeSettings le
  result <- action le'
  _ <- closeScribes le'
  pure result

parseLogLevel :: Text -> Severity
parseLogLevel t = case T.toUpper t of
  "DEBUG"   -> DebugS
  "INFO"    -> InfoS
  "WARNING" -> WarningS
  "ERROR"   -> ErrorS
  _         -> InfoS

logInfo :: (KatipContext m) => Text -> m ()
logInfo msg = katipAddContext () $ logFM InfoS (logStr msg)

logWarn :: (KatipContext m) => Text -> m ()
logWarn msg = katipAddContext () $ logFM WarningS (logStr msg)

logError :: (KatipContext m) => Text -> m ()
logError msg = katipAddContext () $ logFM ErrorS (logStr msg)

logDebug :: (KatipContext m) => Text -> m ()
logDebug msg = katipAddContext () $ logFM DebugS (logStr msg)
