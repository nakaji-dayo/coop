module Coop.Adapter.Dryrun.LLM
  ( mkDryrunLLMOps
  ) where

import Coop.App.Env (LLMOps (..))
import Coop.Domain.LLM (CompletionRequest (..), CompletionResponse (..), Role (..), Message (..))
import qualified Data.Text as T

mkDryrunLLMOps :: (Applicative m) => LLMOps m
mkDryrunLLMOps = LLMOps
  { opsComplete = \req -> pure $ CompletionResponse
      { crResponseText = dryrunResponse req
      }
  }

dryrunResponse :: CompletionRequest -> T.Text
dryrunResponse req =
  let userMsg = case filter (\m -> msgRole m == User) (crMessages req) of
        (m:_) -> msgContent m
        []    -> "no message"
      -- Extract just the "Message:" line if present
      msgLine = case filter ("Message:" `T.isPrefixOf`) (T.lines userMsg) of
        (l:_) -> T.strip $ T.drop 8 l
        []    -> T.take 60 userMsg
      title = T.take 60 msgLine
  in T.unlines
      [ "```json"
      , "{"
      , "  \"priority\": \"Medium\","
      , "  \"title\": \"[DRYRUN] " <> escapeJson title <> "\","
      , "  \"description\": \"Dryrun mode - no actual LLM analysis performed.\","
      , "  \"reason\": \"Dryrun mode: defaulting to Medium priority.\""
      , "}"
      , "```"
      ]

escapeJson :: T.Text -> T.Text
escapeJson = T.replace "\"" "\\\"" . T.replace "\n" " "
