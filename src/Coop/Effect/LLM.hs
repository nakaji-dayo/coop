module Coop.Effect.LLM
  ( LLM (..)
  ) where

import Coop.Domain.LLM (CompletionRequest, CompletionResponse)

class (Monad m) => LLM m where
  complete :: CompletionRequest -> m CompletionResponse
