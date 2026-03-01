module Coop.Adapter.Slack.History
  ( fetchChannelHistory
  ) where

import Data.Aeson (Object, Value (..), eitherDecode)
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Key (fromString)
import qualified Data.Foldable as F
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client

-- | Fetch channel messages since a given timestamp via conversations.history API.
--   Returns messages as raw JSON Objects.
fetchChannelHistory :: Manager -> Text -> Text -> Maybe Text -> IO [Object]
fetchChannelHistory manager botToken channel mOldest =
  go Nothing []
  where
    go :: Maybe Text -> [Object] -> IO [Object]
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
        Left _err -> pure acc
        Right (Object obj) -> do
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
              Nothing -> pure acc'
            else pure acc'
        Right _ -> pure acc
