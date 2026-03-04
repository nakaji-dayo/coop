module Coop.Config
  ( Config (..)
  , RunMode (..)
  , ConnectionMode (..)
  , SlackConfig (..)
  , ClaudeConfig (..)
  , OpenAIConfig (..)
  , LLMConfig (..)
  , NotionConfig (..)
  , DryrunConfig (..)
  , SchedulerConfig (..)
  , GoogleCalendarConfig (..)
  , loadConfig
  , normalizeNotionId
  ) where

import Data.Char (isHexDigit)
import Data.Text (Text)
import qualified Data.Text as T
import Dhall (FromDhall, Generic, Natural, auto, inputFile)
import GHC.Generics ()

data RunMode = Dryrun | Live
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromDhall)

data ConnectionMode = Webhook | SocketMode
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromDhall)

data SlackConfig = SlackConfig
  { slackBotToken          :: Text
  , slackSigningSecret     :: Text
  , slackBotUserId         :: Text
  , slackMonitoredUserId   :: Text
  , slackNotifyChannel     :: Text
  , slackAppToken          :: Text
  , slackConnectionMode    :: ConnectionMode
  , slackCatchupChannels   :: Text  -- ^ Comma-separated channel IDs for offline catch-up
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromDhall)

data ClaudeConfig = ClaudeConfig
  { claudeApiKey  :: Text
  , claudeModel   :: Text
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromDhall)

data OpenAIConfig = OpenAIConfig
  { openaiApiKey :: Text
  , openaiModel  :: Text
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromDhall)

data LLMConfig = LLMConfig
  { llmBackend :: Text        -- ^ "Claude" or "OpenAI"
  , llmClaude  :: ClaudeConfig
  , llmOpenAI  :: OpenAIConfig
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromDhall)

data NotionConfig = NotionConfig
  { notionApiKey          :: Text
  , notionTaskDatabaseId  :: Text
  , notionGuidelinesPageId :: Text
  , notionInstructionsPageId :: Text
  , notionPropName        :: Text
  , notionPropPriority    :: Text
  , notionPropStatus      :: Text
  , notionPropDueDate     :: Text
  , notionStatusOpen      :: Text
  , notionStatusInProgress :: Text
  , notionStatusDone      :: Text
  , notionPropAssignee    :: Text
  , notionAssigneeUserId  :: Text
  , notionPropEstimate    :: Text
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromDhall)

data DryrunConfig = DryrunConfig
  { dryrunDataDir :: Text
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromDhall)

data SchedulerConfig = SchedulerConfig
  { schedulerBriefingCron :: Text  -- ^ cron expression (e.g. "0 9 * * *") or "-" to disable
  , schedulerWeeklyBriefingCron :: Text  -- ^ cron expression for weekly briefing or "-" to disable
  , schedulerWeeklyAvailableHours :: Natural  -- ^ weekly available work hours (e.g. 30)
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromDhall)

data GoogleCalendarConfig = GoogleCalendarConfig
  { googleClientId     :: Text
  , googleClientSecret :: Text
  , googleCalendarId   :: Text
  , googleTokenPath    :: Text
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromDhall)

data Config = Config
  { cfgMode           :: RunMode
  , cfgPort           :: Natural
  , cfgLogLevel       :: Text
  , cfgSlack          :: SlackConfig
  , cfgLLM            :: LLMConfig
  , cfgNotion         :: NotionConfig
  , cfgDryrun         :: DryrunConfig
  , cfgScheduler      :: SchedulerConfig
  , cfgGoogleCalendar :: GoogleCalendarConfig
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromDhall)

loadConfig :: FilePath -> IO Config
loadConfig path = do
  cfg <- inputFile auto path
  let notion = cfgNotion cfg
      notion' = notion
        { notionTaskDatabaseId    = normalizeNotionId (notionTaskDatabaseId notion)
        , notionGuidelinesPageId  = normalizeNotionId (notionGuidelinesPageId notion)
        , notionInstructionsPageId = normalizeNotionId (notionInstructionsPageId notion)
        }
  pure cfg { cfgNotion = notion' }

-- | Normalize a Notion ID field: extract 32-hex ID from a Notion URL,
-- or return the input unchanged if it's already a raw ID.
normalizeNotionId :: Text -> Text
normalizeNotionId input
  | "notion.so/" `T.isInfixOf` input =
      let fragments = drop 1 $ T.splitOn "notion.so/" input
      in case fragments of
           (f:_) -> extractId f
           []    -> input
  | otherwise = input
  where
    extractId fragment =
      let cleaned  = T.takeWhile (\c -> c /= '?' && c /= '#' && c /= ' ' && c /= '\n') fragment
          segments = T.splitOn "/" cleaned
          lastSeg  = if null segments then "" else last segments
          stripped = T.filter (/= '-') lastSeg
          candidate = T.takeEnd 32 stripped
      in if T.length candidate == 32 && T.all isHexDigit candidate
         then candidate
         else input
