module Coop.Adapter.Claude.Client
  ( callClaude
  ) where

import Coop.Adapter.Claude.Types (ClaudeRequest, ClaudeResponse)
import Data.Aeson (encode, eitherDecode)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import qualified Data.ByteString.Lazy as LBS

callClaude :: Manager -> Text -> ClaudeRequest -> IO (Either String ClaudeResponse)
callClaude manager apiKey reqBody = do
  initReq <- parseRequest "https://api.anthropic.com/v1/messages"
  let req = initReq
        { method = "POST"
        , requestHeaders =
            [ ("Content-Type", "application/json")
            , ("x-api-key", TE.encodeUtf8 apiKey)
            , ("anthropic-version", "2023-06-01")
            ]
        , requestBody = RequestBodyLBS (encode reqBody)
        }
  resp <- httpLbs req manager
  let status = statusCode (responseStatus resp)
  if status == 200
    then pure $ eitherDecode (responseBody resp)
    else pure $ Left $ "Claude API error (HTTP " <> show status <> "): "
                     <> show (LBS.toStrict (responseBody resp))
