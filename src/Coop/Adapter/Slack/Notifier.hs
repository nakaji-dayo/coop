module Coop.Adapter.Slack.Notifier
  ( mkLiveNotifierOps
  ) where

import Coop.App.Env (NotifierOps (..))
import Coop.Domain.Notification (Notification (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (encode, object, (.=), decode, withObject, (.:))
import Data.Aeson.Types (parseMaybe, Parser)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LBS
import Katip
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)

mkLiveNotifierOps :: (MonadIO m) => LogEnv -> Text -> Manager -> NotifierOps m
mkLiveNotifierOps logEnv botToken manager = NotifierOps
  { opsNotify = \notif -> liftIO $ do
      slackPostMessage logEnv manager botToken (notifChannel notif) (notifText notif) Nothing

  , opsReplyThread = \channel ts msg -> liftIO $ do
      slackPostMessage logEnv manager botToken channel msg (Just ts)
  }

slackPostMessage :: LogEnv -> Manager -> Text -> Text -> Text -> Maybe Text -> IO ()
slackPostMessage logEnv manager botToken channel text mThreadTs = do
  initReq <- parseRequest "https://slack.com/api/chat.postMessage"
  let body = object $
        [ "channel" .= channel
        , "text" .= text
        ] <> maybe [] (\ts -> ["thread_ts" .= ts]) mThreadTs
      req = initReq
        { method = "POST"
        , requestHeaders =
            [ ("Content-Type", "application/json")
            , ("Authorization", "Bearer " <> TE.encodeUtf8 botToken)
            ]
        , requestBody = RequestBodyLBS (encode body)
        }
  logM logEnv "slack" DebugS $ logStr $ "postMessage to channel=" <> channel
  resp <- httpLbs req manager
  let status = statusCode (responseStatus resp)
      respBody = responseBody resp
  if status /= 200
    then logM logEnv "slack" ErrorS $ logStr $
           "HTTP error: " <> T.pack (show status) <> " " <> T.pack (show (LBS.toStrict respBody))
    else do
      let mError = parseMaybe (withObject "SlackResp" $ \v -> do
            ok <- v .: "ok" :: Parser Bool
            if ok then pure Nothing
            else Just <$> v .: "error") =<< decode respBody
      case mError of
        Just (Just errMsg) ->
          logM logEnv "slack" ErrorS $ logStr $ "API error: " <> (errMsg :: Text)
        _ -> pure ()

logM :: LogEnv -> Namespace -> Severity -> LogStr -> IO ()
logM logEnv ns sev msg =
  runKatipContextT logEnv () ns $ logFM sev msg
