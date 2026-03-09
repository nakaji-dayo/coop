{-# LANGUAGE PatternSynonyms #-}
module Coop.Adapter.Slack.SocketMode
  ( runSlackSocketMode
  ) where

import Coop.Agent.Core (handleSlackEvent)
import Coop.App.Env (Env (..))
import Coop.App.Log (logDebug, logError, logWarn)
import Coop.App.Monad (AppM, runAppM)
import Coop.Config (Config (..), SlackConfig (..))
import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (void)
import Data.Aeson (object, (.=), encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Function ((&))
import Data.Text (pack)
import qualified Slacker
import Slacker
  ( runSocketMode
  , defaultSlackConfig
  , setApiToken
  , setAppToken
  , setOnException
  , handleThreadExceptionSensibly
  , SocketModeEvent
  , pattern EventValue
  )

runSlackSocketMode :: Env AppM -> IO ()
runSlackSocketMode env = do
  let slackCfg = cfgSlack (envConfig env)
      slackerCfg = defaultSlackConfig
                     & setApiToken (slackBotToken slackCfg)
                     & setAppToken (slackAppToken slackCfg)
                     & setOnException (loggingOnException env)
  runSocketMode slackerCfg $ \_cfg evt ->
    handleSocketEvent env evt

loggingOnException :: Env AppM -> Slacker.SlackConfig -> SomeException -> Int -> IO Bool
loggingOnException env cfg ex tId = do
  runAppM env $ logWarn $ "SocketMode exception on thread " <> pack (show tId) <> ": " <> pack (show ex)
  shouldRestart <- handleThreadExceptionSensibly cfg ex tId
  runAppM env $ if shouldRestart
    then logWarn "SocketMode reconnecting..."
    else logError "SocketMode not restarting (fatal exception)"
  pure shouldRestart

handleSocketEvent :: Env AppM -> SocketModeEvent -> IO ()
handleSocketEvent env (EventValue _typ val) = do
  let log_ = runAppM env
  log_ $ logDebug $ "SocketMode EventValue type=" <> pack (show _typ)
  log_ $ logDebug $ "SocketMode payload=" <> pack (BL.unpack (encode val))
  let envelopeValue = object ["event" .= val]
  void $ forkIO $ do
    result <- try $ runAppM env (handleSlackEvent envelopeValue)
    case result of
      Left (e :: SomeException) ->
        log_ $ logError $ "Error processing socket event: " <> pack (show e)
      Right () ->
        log_ $ logDebug "SocketMode event processed successfully"
handleSocketEvent env evt =
  runAppM env $ logDebug $ "SocketMode other event: " <> pack (show evt)
