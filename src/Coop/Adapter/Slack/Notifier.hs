module Coop.Adapter.Slack.Notifier
  ( mkLiveNotifierOps
  ) where

import Coop.App.Env (NotifierOps (..))
import Coop.Domain.Notification (Notification (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (encode, object, (.=))
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)

mkLiveNotifierOps :: (MonadIO m) => Text -> Manager -> NotifierOps m
mkLiveNotifierOps botToken manager = NotifierOps
  { opsNotify = \notif -> liftIO $ do
      slackPostMessage manager botToken (notifChannel notif) (notifText notif) Nothing

  , opsReplyThread = \channel ts msg -> liftIO $ do
      slackPostMessage manager botToken channel msg (Just ts)
  }

slackPostMessage :: Manager -> Text -> Text -> Text -> Maybe Text -> IO ()
slackPostMessage manager botToken channel text mThreadTs = do
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
  resp <- httpLbs req manager
  let status = statusCode (responseStatus resp)
  if status /= 200
    then putStrLn $ "Slack API error: HTTP " <> show status
    else pure ()
