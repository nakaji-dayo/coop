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
  in if "daily briefing" `T.isInfixOf` T.toLower userMsg
     then dryrunBriefingResponse
     else dryrunMentionResponse userMsg

dryrunMentionResponse :: T.Text -> T.Text
dryrunMentionResponse userMsg =
  let msgLine = case filter ("Message:" `T.isPrefixOf`) (T.lines userMsg) of
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

dryrunBriefingResponse :: T.Text
dryrunBriefingResponse = T.unlines
  [ "```json"
  , "{"
  , "  \"schedule\": ["
  , "    { \"task_id\": \"dryrun-task-1\", \"title\": \"[DRYRUN] Sample task\", \"priority\": \"High\", \"reason\": \"Dryrun mode sample\", \"is_must\": true, \"estimate_hours\": 2.0 },"
  , "    { \"task_id\": \"dryrun-task-2\", \"title\": \"[DRYRUN] Another task\", \"priority\": \"Medium\", \"reason\": \"Dryrun mode sample\", \"is_must\": false, \"estimate_hours\": 1.5 }"
  , "  ],"
  , "  \"estimate_requests\": ["
  , "    { \"task_id\": \"dryrun-task-3\", \"title\": \"[DRYRUN] Big task\", \"reason\": \"No estimate provided\" }"
  , "  ],"
  , "  \"summary\": \"[DRYRUN] Today's briefing: 2 tasks scheduled (est. 3.5h), 1 needs estimate.\""
  , "}"
  , "```"
  ]

escapeJson :: T.Text -> T.Text
escapeJson = T.replace "\"" "\\\"" . T.replace "\n" " "
