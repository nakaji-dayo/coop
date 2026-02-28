module Coop.Config
  ( Config (..)
  , RunMode (..)
  , ConnectionMode (..)
  , SlackConfig (..)
  , ClaudeConfig (..)
  , NotionConfig (..)
  , DryrunConfig (..)
  , SchedulerConfig (..)
  , loadConfig
  ) where

import Data.Text (Text)
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
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromDhall)

data ClaudeConfig = ClaudeConfig
  { claudeApiKey  :: Text
  , claudeModel   :: Text
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
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromDhall)

data Config = Config
  { cfgMode      :: RunMode
  , cfgPort      :: Natural
  , cfgLogLevel  :: Text
  , cfgSlack     :: SlackConfig
  , cfgClaude    :: ClaudeConfig
  , cfgNotion    :: NotionConfig
  , cfgDryrun    :: DryrunConfig
  , cfgScheduler :: SchedulerConfig
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromDhall)

loadConfig :: FilePath -> IO Config
loadConfig = inputFile auto
