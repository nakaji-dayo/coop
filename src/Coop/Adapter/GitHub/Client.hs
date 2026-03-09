module Coop.Adapter.GitHub.Client
  ( githubGet
  , githubPost
  , githubPatch
  ) where

import Data.Aeson (FromJSON, ToJSON, encode, eitherDecode)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client
import Network.HTTP.Types.Header (RequestHeaders)
import Network.HTTP.Types.Status (statusCode)
import qualified Data.ByteString.Lazy as LBS

githubHeaders :: Text -> RequestHeaders
githubHeaders token =
  [ ("Authorization", "Bearer " <> TE.encodeUtf8 token)
  , ("Content-Type", "application/json")
  , ("Accept", "application/vnd.github+json")
  , ("User-Agent", "coop-bot")
  , ("X-GitHub-Api-Version", "2022-11-28")
  ]

githubGet :: (FromJSON a) => Manager -> Text -> String -> IO (Either String a)
githubGet manager token url = do
  initReq <- parseRequest url
  let req = initReq
        { method = "GET"
        , requestHeaders = githubHeaders token
        }
  resp <- httpLbs req manager
  let status = statusCode (responseStatus resp)
  if status == 200
    then pure $ eitherDecode (responseBody resp)
    else pure $ Left $ "GitHub API error (HTTP " <> show status <> "): "
                     <> show (LBS.toStrict (responseBody resp))

githubPost :: (ToJSON a, FromJSON b) => Manager -> Text -> String -> a -> IO (Either String b)
githubPost manager token url body = do
  initReq <- parseRequest url
  let req = initReq
        { method = "POST"
        , requestHeaders = githubHeaders token
        , requestBody = RequestBodyLBS (encode body)
        }
  resp <- httpLbs req manager
  let status = statusCode (responseStatus resp)
  if status == 201
    then pure $ eitherDecode (responseBody resp)
    else pure $ Left $ "GitHub API error (HTTP " <> show status <> "): "
                     <> show (LBS.toStrict (responseBody resp))

githubPatch :: (ToJSON a, FromJSON b) => Manager -> Text -> String -> a -> IO (Either String b)
githubPatch manager token url body = do
  initReq <- parseRequest url
  let req = initReq
        { method = "PATCH"
        , requestHeaders = githubHeaders token
        , requestBody = RequestBodyLBS (encode body)
        }
  resp <- httpLbs req manager
  let status = statusCode (responseStatus resp)
  if status == 200
    then pure $ eitherDecode (responseBody resp)
    else pure $ Left $ "GitHub API error (HTTP " <> show status <> "): "
                     <> show (LBS.toStrict (responseBody resp))
