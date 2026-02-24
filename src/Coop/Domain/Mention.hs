module Coop.Domain.Mention
  ( ParsedMention (..)
  , MentionType (..)
  , parseMention
  , stripBotMention
  , isBotMentioned
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

data MentionType = DirectMention | BotCommand
  deriving stock (Eq, Show, Generic)

data ParsedMention = ParsedMention
  { pmChannel      :: Text
  , pmTimestamp    :: Text
  , pmUserId       :: Text
  , pmUserName     :: Maybe Text
  , pmBotUserId    :: Text
  , pmText         :: Text
  , pmStrippedText :: Text
  , pmMentionType  :: MentionType
  } deriving stock (Eq, Show, Generic)

-- | Check if the bot is mentioned in the text.
-- Slack mentions look like <@U12345678>
isBotMentioned :: Text -> Text -> Bool
isBotMentioned botUserId text =
  let mentionTag = "<@" <> botUserId <> ">"
  in  mentionTag `T.isInfixOf` text

-- | Strip the bot mention tag from the text and trim whitespace.
stripBotMention :: Text -> Text -> Text
stripBotMention botUserId text =
  let mentionTag = "<@" <> botUserId <> ">"
  in  T.strip $ T.replace mentionTag "" text

-- | Parse a Slack event into a ParsedMention if the bot is mentioned.
-- Returns Nothing if the bot is not mentioned in the message.
parseMention
  :: Text    -- ^ Bot user ID
  -> Text    -- ^ Channel
  -> Text    -- ^ Timestamp
  -> Text    -- ^ User ID (sender)
  -> Maybe Text  -- ^ User name
  -> Text    -- ^ Message text
  -> Maybe ParsedMention
parseMention botUserId channel ts userId userName text
  | not (isBotMentioned botUserId text) = Nothing
  | otherwise =
      let stripped = stripBotMention botUserId text
          mentionType = if T.null stripped then DirectMention else BotCommand
      in  Just ParsedMention
            { pmChannel      = channel
            , pmTimestamp    = ts
            , pmUserId       = userId
            , pmUserName     = userName
            , pmBotUserId    = botUserId
            , pmText         = text
            , pmStrippedText = stripped
            , pmMentionType  = mentionType
            }
