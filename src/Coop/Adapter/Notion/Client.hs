module Coop.Adapter.Notion.Client
  ( notionGet
  , notionPost
  , notionPatch
  ) where

import Data.Aeson (FromJSON, ToJSON, encode, eitherDecode)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client
import Network.HTTP.Types.Header (RequestHeaders)
import Network.HTTP.Types.Status (statusCode)
import qualified Data.ByteString.Lazy as LBS

notionHeaders :: Text -> RequestHeaders
notionHeaders apiKey =
  [ ("Authorization", "Bearer " <> TE.encodeUtf8 apiKey)
  , ("Content-Type", "application/json")
  , ("Notion-Version", "2022-06-28")
  ]

notionGet :: (FromJSON a) => Manager -> Text -> String -> IO (Either String a)
notionGet manager apiKey url = do
  initReq <- parseRequest url
  let req = initReq
        { method = "GET"
        , requestHeaders = notionHeaders apiKey
        }
  resp <- httpLbs req manager
  let status = statusCode (responseStatus resp)
  if status == 200
    then pure $ eitherDecode (responseBody resp)
    else pure $ Left $ "Notion API error (HTTP " <> show status <> "): "
                     <> show (LBS.toStrict (responseBody resp))

notionPost :: (ToJSON a, FromJSON b) => Manager -> Text -> String -> a -> IO (Either String b)
notionPost manager apiKey url body = do
  initReq <- parseRequest url
  let req = initReq
        { method = "POST"
        , requestHeaders = notionHeaders apiKey
        , requestBody = RequestBodyLBS (encode body)
        }
  resp <- httpLbs req manager
  let status = statusCode (responseStatus resp)
  if status == 200
    then pure $ eitherDecode (responseBody resp)
    else pure $ Left $ "Notion API error (HTTP " <> show status <> "): "
                     <> show (LBS.toStrict (responseBody resp))

notionPatch :: (ToJSON a, FromJSON b) => Manager -> Text -> String -> a -> IO (Either String b)
notionPatch manager apiKey url body = do
  initReq <- parseRequest url
  let req = initReq
        { method = "PATCH"
        , requestHeaders = notionHeaders apiKey
        , requestBody = RequestBodyLBS (encode body)
        }
  resp <- httpLbs req manager
  let status = statusCode (responseStatus resp)
  if status == 200
    then pure $ eitherDecode (responseBody resp)
    else pure $ Left $ "Notion API error (HTTP " <> show status <> "): "
                     <> show (LBS.toStrict (responseBody resp))
