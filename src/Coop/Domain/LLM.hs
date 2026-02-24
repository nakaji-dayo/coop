module Coop.Domain.LLM
  ( CompletionRequest (..)
  , CompletionResponse (..)
  , Role (..)
  , Message (..)
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)

data Role = System | User | Assistant
  deriving stock (Eq, Show, Generic)

data Message = Message
  { msgRole    :: Role
  , msgContent :: Text
  } deriving stock (Eq, Show, Generic)

data CompletionRequest = CompletionRequest
  { crSystem   :: Maybe Text
  , crMessages :: [Message]
  } deriving stock (Eq, Show, Generic)

data CompletionResponse = CompletionResponse
  { crResponseText :: Text
  } deriving stock (Eq, Show, Generic)
