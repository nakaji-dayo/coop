module Coop.Adapter.OpenAI.LLM
  ( mkOpenAILLMOps
  ) where

import Coop.Adapter.OpenAI.Client (callOpenAI)
import Coop.Adapter.OpenAI.Types (OpenAIRequest (..), OpenAIResponse (..), OpenAIMessage (..), OpenAIChoice (..))
import Coop.App.Env (LLMOps (..))
import Coop.Config (OpenAIConfig (..))
import Coop.Domain.LLM (CompletionRequest (..), CompletionResponse (..), Role (..), Message (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text as T
import Network.HTTP.Client (Manager)

mkOpenAILLMOps :: (MonadIO m) => OpenAIConfig -> Manager -> LLMOps m
mkOpenAILLMOps config manager = LLMOps
  { opsComplete = \req -> liftIO $ do
      let oaiReq = toOpenAIRequest config req
      result <- callOpenAI manager (openaiApiKey config) oaiReq
      case result of
        Left err -> pure $ CompletionResponse
          { crResponseText = "Error calling OpenAI API: " <> T.pack err }
        Right resp -> pure $ fromOpenAIResponse resp
  }

toOpenAIRequest :: OpenAIConfig -> CompletionRequest -> OpenAIRequest
toOpenAIRequest config req = OpenAIRequest
  { oarqModel     = openaiModel config
  , oarqMessages  = sysMsg <> map toOpenAIMessage (crMessages req)
  , oarqMaxTokens = Just 4096
  }
  where
    sysMsg = case crSystem req of
      Nothing -> []
      Just s  -> [OpenAIMessage "system" s]

toOpenAIMessage :: Message -> OpenAIMessage
toOpenAIMessage msg = OpenAIMessage
  { oamRole    = roleToText (msgRole msg)
  , oamContent = msgContent msg
  }

roleToText :: Role -> T.Text
roleToText System    = "system"
roleToText User      = "user"
roleToText Assistant = "assistant"

fromOpenAIResponse :: OpenAIResponse -> CompletionResponse
fromOpenAIResponse resp = CompletionResponse
  { crResponseText = case oarspChoices resp of
      (c:_) -> oamContent (oacMessage c)
      []    -> ""
  }
