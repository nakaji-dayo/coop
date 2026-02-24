module Coop.Adapter.Claude.Types
  ( ClaudeRequest (..)
  , ClaudeResponse (..)
  , ClaudeMessage (..)
  , ClaudeContent (..)
  , ClaudeUsage (..)
  ) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

data ClaudeMessage = ClaudeMessage
  { cmRole    :: Text
  , cmContent :: Text
  } deriving stock (Eq, Show, Generic)

instance ToJSON ClaudeMessage where
  toJSON (ClaudeMessage role content) = object
    [ "role" .= role
    , "content" .= content
    ]

instance FromJSON ClaudeMessage where
  parseJSON = withObject "ClaudeMessage" $ \v -> ClaudeMessage
    <$> v .: "role"
    <*> v .: "content"

data ClaudeRequest = ClaudeRequest
  { crqModel     :: Text
  , crqMaxTokens :: Int
  , crqSystem    :: Maybe Text
  , crqMessages  :: [ClaudeMessage]
  } deriving stock (Eq, Show, Generic)

instance ToJSON ClaudeRequest where
  toJSON (ClaudeRequest model maxTokens sys msgs) =
    let base = [ "model" .= model
               , "max_tokens" .= maxTokens
               , "messages" .= msgs
               ]
        sysField = maybe [] (\s -> ["system" .= s]) sys
    in object (base <> sysField)

data ClaudeContent = ClaudeContent
  { ccType :: Text
  , ccText :: Text
  } deriving stock (Eq, Show, Generic)

instance FromJSON ClaudeContent where
  parseJSON = withObject "ClaudeContent" $ \v -> ClaudeContent
    <$> v .: "type"
    <*> v .: "text"

data ClaudeUsage = ClaudeUsage
  { cuInputTokens  :: Int
  , cuOutputTokens :: Int
  } deriving stock (Eq, Show, Generic)

instance FromJSON ClaudeUsage where
  parseJSON = withObject "ClaudeUsage" $ \v -> ClaudeUsage
    <$> v .: "input_tokens"
    <*> v .: "output_tokens"

data ClaudeResponse = ClaudeResponse
  { crspId      :: Text
  , crspContent :: [ClaudeContent]
  , crspModel   :: Text
  , crspUsage   :: ClaudeUsage
  } deriving stock (Eq, Show, Generic)

instance FromJSON ClaudeResponse where
  parseJSON = withObject "ClaudeResponse" $ \v -> ClaudeResponse
    <$> v .: "id"
    <*> v .: "content"
    <*> v .: "model"
    <*> v .: "usage"
