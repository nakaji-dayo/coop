module Coop.Adapter.OpenAI.Types
  ( OpenAIRequest (..)
  , OpenAIResponse (..)
  , OpenAIMessage (..)
  , OpenAIChoice (..)
  , OpenAIUsage (..)
  ) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

data OpenAIMessage = OpenAIMessage
  { oamRole    :: Text
  , oamContent :: Text
  } deriving stock (Eq, Show, Generic)

instance ToJSON OpenAIMessage where
  toJSON (OpenAIMessage role content) = object
    [ "role" .= role
    , "content" .= content
    ]

instance FromJSON OpenAIMessage where
  parseJSON = withObject "OpenAIMessage" $ \v -> OpenAIMessage
    <$> v .: "role"
    <*> v .: "content"

data OpenAIRequest = OpenAIRequest
  { oarqModel     :: Text
  , oarqMessages  :: [OpenAIMessage]
  , oarqMaxTokens :: Maybe Int
  } deriving stock (Eq, Show, Generic)

instance ToJSON OpenAIRequest where
  toJSON (OpenAIRequest model msgs maxTok) =
    let base = [ "model" .= model, "messages" .= msgs ]
        mtField = maybe [] (\n -> ["max_tokens" .= n]) maxTok
    in object (base <> mtField)

data OpenAIChoice = OpenAIChoice
  { oacIndex        :: Int
  , oacMessage      :: OpenAIMessage
  , oacFinishReason :: Maybe Text
  } deriving stock (Eq, Show, Generic)

instance FromJSON OpenAIChoice where
  parseJSON = withObject "OpenAIChoice" $ \v -> OpenAIChoice
    <$> v .: "index"
    <*> v .: "message"
    <*> v .:? "finish_reason"

data OpenAIUsage = OpenAIUsage
  { oauPromptTokens     :: Int
  , oauCompletionTokens :: Int
  , oauTotalTokens      :: Int
  } deriving stock (Eq, Show, Generic)

instance FromJSON OpenAIUsage where
  parseJSON = withObject "OpenAIUsage" $ \v -> OpenAIUsage
    <$> v .: "prompt_tokens"
    <*> v .: "completion_tokens"
    <*> v .: "total_tokens"

data OpenAIResponse = OpenAIResponse
  { oarspId      :: Text
  , oarspChoices :: [OpenAIChoice]
  , oarspUsage   :: OpenAIUsage
  } deriving stock (Eq, Show, Generic)

instance FromJSON OpenAIResponse where
  parseJSON = withObject "OpenAIResponse" $ \v -> OpenAIResponse
    <$> v .: "id"
    <*> v .: "choices"
    <*> v .: "usage"
