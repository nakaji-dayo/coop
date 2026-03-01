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
      lower = T.toLower userMsg
  in if "weekly briefing" `T.isInfixOf` lower
     then dryrunWeeklyBriefingResponse
     else if "daily briefing" `T.isInfixOf` lower
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
  , "  \"meeting_preps\": ["
  , "    { \"title\": \"Design Review\", \"reason\": \"Review the latest design mockups and prepare feedback\" }"
  , "  ],"
  , "  \"meeting_hours\": 1.5,"
  , "  \"summary\": \"[DRYRUN] Today's briefing: 2 tasks scheduled (est. 3.5h), 1.5h meetings, 1 needs estimate.\""
  , "}"
  , "```"
  ]

dryrunWeeklyBriefingResponse :: T.Text
dryrunWeeklyBriefingResponse = T.unlines
  [ "```json"
  , "{"
  , "  \"long_term_milestones\": ["
  , "    { \"goal\": \"[DRYRUN] Ship v2.0 release\", \"timeframe\": \"1 month\", \"key_tasks\": [\"[DRYRUN] Sample task\", \"[DRYRUN] Another task\"] },"
  , "    { \"goal\": \"[DRYRUN] Improve test coverage\", \"timeframe\": \"2-3 months\", \"key_tasks\": [\"[DRYRUN] Another task\"] }"
  , "  ],"
  , "  \"weekly_tasks\": ["
  , "    { \"task_id\": \"dryrun-task-1\", \"title\": \"[DRYRUN] Sample task\", \"priority\": \"High\", \"estimate_hours\": 8.0, \"milestone_link\": \"Supports v2.0 release\", \"reason\": \"Dryrun mode sample\" },"
  , "    { \"task_id\": \"dryrun-task-2\", \"title\": \"[DRYRUN] Another task\", \"priority\": \"Medium\", \"estimate_hours\": 4.0, \"milestone_link\": \"Improves test coverage\", \"reason\": \"Dryrun mode sample\" }"
  , "  ],"
  , "  \"guideline_feedback\": \"[DRYRUN] Current guidelines look good. Consider adding a section on code review turnaround time.\","
  , "  \"summary\": \"[DRYRUN] This week: focus on v2.0 release tasks (12h planned out of 30h available).\""
  , "}"
  , "```"
  ]

escapeJson :: T.Text -> T.Text
escapeJson = T.replace "\"" "\\\"" . T.replace "\n" " "
