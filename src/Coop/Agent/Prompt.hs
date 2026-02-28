module Coop.Agent.Prompt
  ( buildMentionAnalysisPrompt
  , buildBotCommandPrompt
  , buildDailyBriefingPrompt
  , MentionAnalysis (..)
  , AnalysisResult (..)
  , DailyBriefing (..)
  , BriefingTask (..)
  , EstimateRequest (..)
  , MeetingPrep (..)
  , parseMentionAnalysis
  , parseDailyBriefing
  ) where

import Coop.Agent.Context (AgentContext (..))
import Coop.Domain.Calendar (CalendarEvent (..), ResponseStatus (..))
import Coop.Domain.LLM (CompletionRequest (..), Message (..), Role (..))
import Coop.Domain.Mention (ParsedMention (..))
import Coop.Domain.Task (Task (..), TaskId (..), Priority (..), TaskStatus (..))
import Data.Aeson (FromJSON (..), withObject, (.:), (.:?), eitherDecodeStrict)
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Map.Strict as Map
import Data.Time (Day, TimeZone, utcToLocalTime, formatTime, defaultTimeLocale)
import GHC.Generics (Generic)

data MentionAnalysis = MentionAnalysis
  { maPriority    :: Priority
  , maTitle       :: Text
  , maDescription :: Text
  , maReason      :: Text
  , maDueDate     :: Maybe Day
  } deriving stock (Eq, Show, Generic)

data AnalysisResult
  = CreateTask MentionAnalysis
  | SkipTask Text  -- ^ reason for skipping
  deriving stock (Eq, Show)

instance FromJSON AnalysisResult where
  parseJSON = withObject "AnalysisResult" $ \v -> do
    action <- v .: "action" :: Parser Text
    reason <- v .: "reason"
    case action of
      "skip" -> pure $ SkipTask reason
      _ -> do
        prio <- v .: "priority"
        title <- v .: "title"
        desc <- v .: "description"
        dueDate <- v .:? "due_date"
        priority <- case (prio :: Text) of
          "Critical" -> pure Critical
          "High"     -> pure High
          "Medium"   -> pure Medium
          "Low"      -> pure Low
          _          -> pure Medium
        pure $ CreateTask $ MentionAnalysis priority title desc reason dueDate

parseMentionAnalysis :: Text -> Either String AnalysisResult
parseMentionAnalysis txt =
  let cleaned = extractJson txt
  in eitherDecodeStrict (TE.encodeUtf8 cleaned)

extractJson :: Text -> Text
extractJson txt
  | "```json" `T.isInfixOf` txt =
      let afterStart = T.drop 1 $ snd $ T.breakOn "\n" $ snd $ T.breakOn "```json" txt
          beforeEnd = fst $ T.breakOn "```" afterStart
      in T.strip beforeEnd
  | "{" `T.isInfixOf` txt =
      let afterBrace = snd $ T.breakOn "{" txt
          -- Find the matching closing brace (simple heuristic)
          (json, _) = T.breakOnEnd "}" afterBrace
      in T.strip json
  | otherwise = txt

buildMentionAnalysisPrompt :: AgentContext -> Day -> ParsedMention -> CompletionRequest
buildMentionAnalysisPrompt ctx today mention = CompletionRequest
  { crSystem = Just systemPrompt
  , crMessages =
      [ Message User userContent
      ]
  }
  where
    systemPrompt = T.unlines
      [ acInstructions ctx
      , ""
      , "## Behavioral Guidelines"
      , acGuidelines ctx
      , ""
      , "## Existing Tasks"
      , formatTasks (acExistingTasks ctx)
      ]

    userContent = T.unlines
      [ "A new mention was received:"
      , ""
      , "Today: " <> T.pack (show today)
      , "Channel: " <> pmChannel mention
      , "User: " <> pmUserId mention <> maybe "" (" (" <>) (pmUserName mention) <> maybe "" (const ")") (pmUserName mention)
      , "Message: " <> pmStrippedText mention
      , ""
      , "Analyze this mention and respond with a JSON object."
      , ""
      , "If this should be turned into a task:"
      , "{ \"action\": \"create_task\", \"priority\": \"Critical|High|Medium|Low\", \"title\": \"...\", \"description\": \"...\", \"reason\": \"...\", \"due_date\": \"YYYY-MM-DD or null\" }"
      , ""
      , "Set due_date when the message implies a deadline (e.g. \"明日までに\" → tomorrow's date, \"今週中\" → end of this week). Set null if no deadline is mentioned."
      , ""
      , "If this should NOT be a task (e.g. casual conversation, greetings, etc.):"
      , "{ \"action\": \"skip\", \"reason\": \"...\" }"
      , ""
      , "IMPORTANT: Write all JSON field values (title, description, reason) following the tone and style specified in the system instructions above."
      ]

buildBotCommandPrompt :: AgentContext -> ParsedMention -> CompletionRequest
buildBotCommandPrompt ctx mention = CompletionRequest
  { crSystem = Just systemPrompt
  , crMessages =
      [ Message User (pmStrippedText mention)
      ]
  }
  where
    systemPrompt = T.unlines
      [ acInstructions ctx
      , ""
      , "## Behavioral Guidelines"
      , acGuidelines ctx
      , ""
      , "## Current Tasks"
      , formatTasks (acExistingTasks ctx)
      , ""
      , "The user is talking to you directly. Respond helpfully and concisely."
      , "If they ask to re-evaluate a task priority, provide updated analysis."
      ]

formatTasks :: [Task] -> Text
formatTasks [] = "No existing tasks."
formatTasks tasks = T.unlines $ map formatTask tasks

formatTask :: Task -> Text
formatTask task = "- [" <> prioText <> "] " <> taskTitle task <> ": " <> taskDescription task
  where
    prioText = case taskPriority task of
      Critical -> "CRITICAL"
      High     -> "HIGH"
      Medium   -> "MEDIUM"
      Low      -> "LOW"

-- Daily Briefing types

data MeetingPrep = MeetingPrep
  { mpTitle  :: Text
  , mpReason :: Text
  } deriving stock (Eq, Show, Generic)

instance FromJSON MeetingPrep where
  parseJSON = withObject "MeetingPrep" $ \v ->
    MeetingPrep
      <$> v .: "title"
      <*> v .: "reason"

data DailyBriefing = DailyBriefing
  { dbSchedule        :: [BriefingTask]
  , dbEstimateRequests :: [EstimateRequest]
  , dbMeetingPreps    :: [MeetingPrep]
  , dbMeetingHours    :: Double
  , dbSummary         :: Text
  } deriving stock (Eq, Show, Generic)

data BriefingTask = BriefingTask
  { btTaskId        :: Text
  , btTitle         :: Text
  , btPriority      :: Text
  , btReason        :: Text
  , btIsMust        :: Bool
  , btEstimateHours :: Maybe Double
  } deriving stock (Eq, Show, Generic)

data EstimateRequest = EstimateRequest
  { erTaskId :: Text
  , erTitle  :: Text
  , erReason :: Text
  } deriving stock (Eq, Show, Generic)

instance FromJSON DailyBriefing where
  parseJSON = withObject "DailyBriefing" $ \v ->
    DailyBriefing
      <$> v .: "schedule"
      <*> (v .:? "estimate_requests" >>= pure . maybe [] id)
      <*> (v .:? "meeting_preps" >>= pure . maybe [] id)
      <*> (v .:? "meeting_hours" >>= pure . maybe 0 id)
      <*> v .: "summary"

instance FromJSON BriefingTask where
  parseJSON = withObject "BriefingTask" $ \v ->
    BriefingTask
      <$> v .: "task_id"
      <*> v .: "title"
      <*> v .: "priority"
      <*> v .: "reason"
      <*> v .: "is_must"
      <*> v .:? "estimate_hours"

instance FromJSON EstimateRequest where
  parseJSON = withObject "EstimateRequest" $ \v ->
    EstimateRequest
      <$> v .: "task_id"
      <*> v .: "title"
      <*> v .: "reason"

parseDailyBriefing :: Text -> Either String DailyBriefing
parseDailyBriefing txt =
  let cleaned = extractJson txt
  in eitherDecodeStrict (TE.encodeUtf8 cleaned)

buildDailyBriefingPrompt :: AgentContext -> Day -> TimeZone -> CompletionRequest
buildDailyBriefingPrompt ctx today tz = CompletionRequest
  { crSystem = Just systemPrompt
  , crMessages = [ Message User userContent ]
  }
  where
    systemPrompt = T.unlines
      [ acInstructions ctx
      , ""
      , "## Behavioral Guidelines"
      , acGuidelines ctx
      ]

    activeTasks = filter isActive (acExistingTasks ctx)
    isActive t = taskStatus t == Open || taskStatus t == InProgress

    bodies = acTaskBodies ctx
    events = filter (not . isDeclined) (acCalendarEvents ctx)

    userContent = T.unlines
      [ "Generate a daily briefing for today."
      , ""
      , "Today: " <> T.pack (show today)
      , ""
      , "## Today's Calendar"
      , if null events then "No events scheduled."
        else T.unlines (map (formatCalendarEvent tz) events)
      , ""
      , "## Current Tasks"
      , if null activeTasks then "No active tasks."
        else T.unlines (map (formatBriefingInput bodies) activeTasks)
      , ""
      , "Create a daily schedule. Respond with a JSON object:"
      , "{"
      , "  \"schedule\": ["
      , "    { \"task_id\": \"...\", \"title\": \"...\", \"priority\": \"High\", \"reason\": \"Why today\", \"is_must\": true, \"estimate_hours\": 2.0 }"
      , "  ],"
      , "  \"estimate_requests\": ["
      , "    { \"task_id\": \"...\", \"title\": \"...\", \"reason\": \"Why estimate needed\" }"
      , "  ],"
      , "  \"meeting_preps\": ["
      , "    { \"title\": \"Meeting name\", \"reason\": \"What to prepare\" }"
      , "  ],"
      , "  \"meeting_hours\": 2.5,"
      , "  \"summary\": \"Brief overview of today's focus\""
      , "}"
      , ""
      , "Rules:"
      , "- Include only tasks that should be worked on TODAY"
      , "- is_must=true for tasks that absolutely must be done today (due today, critical priority, blockers)"
      , "- is_must=false for tasks that ideally should be worked on but can slip"
      , "- estimate_hours: estimated hours to complete the task. If the task already has an Estimate value, convert it to hours. Otherwise, estimate based on the task title, body content, and linked documents"
      , "- meeting_hours: from Today's Calendar above, determine which events are actual meetings (e.g. standup, 1on1, review) and sum their durations. Do NOT count non-meeting events such as focus/work blocks, personal events, reminders, lunch breaks, or time-off. Set to 0 if no meetings"
      , "- Today's base work capacity is 8h. Available work time = 8h - meeting_hours. The total scheduled task hours must fit within the available time. Be realistic"
      , "- Do NOT schedule tasks during meeting times — schedule work around meetings"
      , "- Declined events are already excluded from the calendar above"
      , "- meeting_preps: suggest preparation for meetings that seem to need it (e.g. 1on1s, design reviews, presentations). Include the meeting title and what to prepare"
      , "- estimate_requests: list tasks NOT in today's schedule that seem large but have no estimate"
      , "- IMPORTANT: Write all JSON field values (summary, reason) following the tone and style specified in the system instructions above."
      ]

isDeclined :: CalendarEvent -> Bool
isDeclined e = calResponseStatus e == Declined

formatCalendarEvent :: TimeZone -> CalendarEvent -> Text
formatCalendarEvent tz e =
  let startLocal = utcToLocalTime tz (calStart e)
      endLocal = utcToLocalTime tz (calEnd e)
      timeFmt = formatTime defaultTimeLocale "%H:%M"
      statusText = case calResponseStatus e of
        Accepted    -> "Accepted"
        Tentative   -> "Tentative"
        NeedsAction -> "NeedsAction"
        Declined    -> "Declined"
  in T.concat
    [ T.pack (timeFmt startLocal)
    , "-"
    , T.pack (timeFmt endLocal)
    , " | "
    , calTitle e
    , " ("
    , statusText
    , ")"
    ]

formatBriefingInput :: Map.Map Text Text -> Task -> Text
formatBriefingInput bodies task =
  let base = T.intercalate " | "
        [ "ID:" <> unTaskId (taskId task)
        , "Title:" <> taskTitle task
        , "Priority:" <> prioText
        , "Status:" <> statusText
        , "Due:" <> maybe "none" (T.pack . show) (taskDueDate task)
        , "Estimate:" <> maybe "none" id (taskEstimate task)
        ]
      bodySection = case Map.lookup (unTaskId (taskId task)) bodies of
        Just content | not (T.null content) ->
          "\n  Body: " <> T.take 2000 (T.replace "\n" " " content)
        _ -> ""
  in base <> bodySection
  where
    prioText = case taskPriority task of
      Critical -> "Critical"
      High     -> "High"
      Medium   -> "Medium"
      Low      -> "Low"
    statusText = case taskStatus task of
      Open       -> "Open"
      InProgress -> "InProgress"
      Done       -> "Done"
      Archived   -> "Archived"
