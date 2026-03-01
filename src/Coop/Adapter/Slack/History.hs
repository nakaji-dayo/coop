module Coop.Adapter.Slack.History
  ( fetchChannelHistory
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson (Object, Value (..), eitherDecode)
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Key (fromString)
import qualified Data.Foldable as F
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client

-- | Fetch channel messages since a given timestamp via conversations.history API.
--   Returns messages as raw JSON Objects, or an error description on failure.
fetchChannelHistory :: Manager -> Text -> Text -> Maybe Text -> IO (Either Text [Object])
fetchChannelHistory manager botToken channel mOldest = do
  result <- try (go Nothing [])
  case result of
    Left (e :: SomeException) -> pure $ Left $ "HTTP error: " <> T.pack (show e)
    Right v -> pure v
  where
    go :: Maybe Text -> [Object] -> IO (Either Text [Object])
    go cursor acc = do
      initReq <- parseRequest "https://slack.com/api/conversations.history"
      let qparams =
            [ ("channel", Just (TE.encodeUtf8 channel))
            , ("limit", Just "200")
            ]
            <> maybe [] (\ts -> [("oldest", Just (TE.encodeUtf8 ts))]) mOldest
            <> maybe [] (\c -> [("cursor", Just (TE.encodeUtf8 c))]) cursor
          req = setQueryString qparams $ initReq
            { method = "GET"
            , requestHeaders =
                [ ("Authorization", "Bearer " <> TE.encodeUtf8 botToken)
                ]
            }
      resp <- httpLbs req manager
      case eitherDecode (responseBody resp) of
        Left err -> pure $ Left $ "JSON decode error: " <> T.pack err
        Right (Object obj) ->
          case KM.lookup "ok" obj of
            Just (Bool False) ->
              let apiErr = case KM.lookup (fromString "error") obj of
                    Just (String e) -> e
                    _ -> "unknown"
              in pure $ Left $ "Slack API error: " <> apiErr
            _ -> do
              let msgs = case KM.lookup "messages" obj of
                    Just (Array arr) -> [o | Object o <- F.toList arr]
                    _ -> []
                  hasMore = case KM.lookup "has_more" obj of
                    Just (Bool True) -> True
                    _ -> False
                  nextCursor = case KM.lookup "response_metadata" obj of
                    Just (Object meta) -> case KM.lookup (fromString "next_cursor") meta of
                      Just (String c) | not (T.null c) -> Just c
                      _ -> Nothing
                    _ -> Nothing
                  acc' = acc <> msgs
              if hasMore
                then case nextCursor of
                  Just c  -> go (Just c) acc'
                  Nothing -> pure $ Right acc'
                else pure $ Right acc'
        Right _ -> pure $ Left "Unexpected JSON response (not an object)"
