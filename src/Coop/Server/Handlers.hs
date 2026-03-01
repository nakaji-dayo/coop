module Coop.Server.Handlers
  ( mkApp
  , mkHealthApp
  ) where

import Coop.App.Env (Env (..))
import Coop.App.Monad (AppM (..), runAppM)
import Coop.App.Log (logInfo, logError, logWarn)
import Coop.Config (Config (..), SlackConfig (..))
import Coop.Server.API (api, healthApi)
import Coop.Agent.Core (handleSlackEvent)
import Control.Concurrent (forkIO)
import Control.Exception (try, SomeException)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson (Value (..), decode)
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Servant
import Crypto.MAC.HMAC (hmac, HMAC)
import Crypto.Hash (SHA256)
import Data.ByteArray (convert)
import qualified Data.ByteArray.Encoding as BAE

mkApp :: Env AppM -> Application
mkApp env = serve api (hoistServer api (appMToHandler env) handlers)
  where
    handlers = healthHandler :<|> slackEventsHandler

mkHealthApp :: Env AppM -> Application
mkHealthApp env = serve healthApi (hoistServer healthApi (appMToHandler env) healthHandler)

appMToHandler :: Env AppM -> AppM a -> Handler a
appMToHandler env m = do
  result <- liftIO $ try $ runAppM env m
  case result of
    Left (e :: SomeException) ->
      throwError $ err500 { errBody = LBS.fromStrict (TE.encodeUtf8 (T.pack (show e))) }
    Right a -> pure a

healthHandler :: AppM Value
healthHandler = do
  logInfo "Health check"
  pure $ Object $ KM.fromList [("status", String "ok")]

slackEventsHandler :: Maybe T.Text -> Maybe T.Text -> ByteString -> AppM Value
slackEventsHandler mSig mTimestamp body = do
  config <- asks envConfig
  let signingSecret = slackSigningSecret (cfgSlack config)

  case (mSig, mTimestamp) of
    (Just sig, Just ts) -> do
      if verifySlackSignature signingSecret ts body sig
        then handleEventRequest body
        else do
          logWarn "Invalid Slack signature"
          pure $ Object $ KM.fromList [("error", String "invalid signature")]
    _ -> do
      logWarn "Missing Slack signature headers, proceeding anyway"
      handleEventRequest body

handleEventRequest :: ByteString -> AppM Value
handleEventRequest body =
  case decode (LBS.fromStrict body) :: Maybe Value of
    Nothing -> do
      logError "Failed to parse Slack event JSON"
      pure $ Object $ KM.fromList [("error", String "invalid json")]
    Just val@(Object obj) ->
      case KM.lookup "type" obj of
        Just (String "url_verification") -> do
          logInfo "URL verification challenge received"
          let challenge = maybe Null id (KM.lookup "challenge" obj)
          pure $ Object $ KM.fromList [("challenge", challenge)]

        Just (String "event_callback") -> do
          logInfo "Event callback received"
          -- Respond 200 immediately, process asynchronously (Slack 3s timeout)
          env <- asks id
          liftIO $ void $ forkIO $ do
            result <- try $ runAppM env (handleSlackEvent val)
            case result of
              Left (e :: SomeException) ->
                putStrLn $ "Error processing Slack event: " <> show e
              Right () -> pure ()
          pure $ Object $ KM.fromList [("ok", Bool True)]

        _ -> do
          logInfo "Unknown event type"
          pure $ Object $ KM.fromList [("ok", Bool True)]
    Just _ -> do
      logError "Unexpected JSON value (not object)"
      pure $ Object $ KM.fromList [("error", String "expected object")]

verifySlackSignature :: T.Text -> T.Text -> ByteString -> T.Text -> Bool
verifySlackSignature secret timestamp body signature =
  let basestring = "v0:" <> TE.encodeUtf8 timestamp <> ":" <> body
      secretBytes = TE.encodeUtf8 secret
      mac = hmac secretBytes basestring :: HMAC SHA256
      hexDigest = BAE.convertToBase BAE.Base16 (convert mac :: ByteString)
      expected = "v0=" <> hexDigest
  in  TE.encodeUtf8 signature == expected
