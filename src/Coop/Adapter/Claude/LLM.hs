module Coop.Adapter.Claude.LLM
  ( mkLiveLLMOps
  ) where

import Coop.Adapter.Claude.Client (callClaude)
import Coop.Adapter.Claude.Types (ClaudeRequest (..), ClaudeResponse (..), ClaudeMessage (..), ClaudeContent (..))
import Coop.App.Env (LLMOps (..))
import Coop.Config (ClaudeConfig (..))
import Coop.Domain.LLM (CompletionRequest (..), CompletionResponse (..), Role (..), Message (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client (Manager)

mkLiveLLMOps :: (MonadIO m) => ClaudeConfig -> Manager -> LLMOps m
mkLiveLLMOps config manager = LLMOps
  { opsComplete = \req -> liftIO $ do
      let claudeReq = toClaudeRequest config req
      result <- callClaude manager (claudeApiKey config) claudeReq
      case result of
        Left err -> pure $ CompletionResponse
          { crResponseText = "Error calling Claude API: " <> T.pack err }
        Right resp -> pure $ fromClaudeResponse resp
  }

toClaudeRequest :: ClaudeConfig -> CompletionRequest -> ClaudeRequest
toClaudeRequest config req = ClaudeRequest
  { crqModel     = claudeModel config
  , crqMaxTokens = 4096
  , crqSystem    = crSystem req
  , crqMessages  = map toClaudeMessage (crMessages req)
  }

toClaudeMessage :: Message -> ClaudeMessage
toClaudeMessage msg = ClaudeMessage
  { cmRole    = roleToText (msgRole msg)
  , cmContent = msgContent msg
  }

roleToText :: Role -> Text
roleToText System    = "user"  -- Claude API uses system param, not role
roleToText User      = "user"
roleToText Assistant  = "assistant"

fromClaudeResponse :: ClaudeResponse -> CompletionResponse
fromClaudeResponse resp = CompletionResponse
  { crResponseText = T.intercalate "\n" $ map ccText (crspContent resp)
  }
