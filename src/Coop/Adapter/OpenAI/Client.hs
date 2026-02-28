module Coop.Adapter.OpenAI.Client
  ( callOpenAI
  ) where

import Coop.Adapter.OpenAI.Types (OpenAIRequest, OpenAIResponse)
import Data.Aeson (encode, eitherDecode)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import qualified Data.ByteString.Lazy as LBS

callOpenAI :: Manager -> Text -> OpenAIRequest -> IO (Either String OpenAIResponse)
callOpenAI manager apiKey reqBody = do
  initReq <- parseRequest "https://api.openai.com/v1/chat/completions"
  let req = initReq
        { method = "POST"
        , requestHeaders =
            [ ("Content-Type", "application/json")
            , ("Authorization", "Bearer " <> TE.encodeUtf8 apiKey)
            ]
        , requestBody = RequestBodyLBS (encode reqBody)
        }
  resp <- httpLbs req manager
  let status = statusCode (responseStatus resp)
  if status == 200
    then pure $ eitherDecode (responseBody resp)
    else pure $ Left $ "OpenAI API error (HTTP " <> show status <> "): "
                     <> show (LBS.toStrict (responseBody resp))
