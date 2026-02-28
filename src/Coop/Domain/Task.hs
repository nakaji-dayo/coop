module Coop.Domain.Task
  ( Task (..)
  , TaskId (..)
  , Priority (..)
  , TaskStatus (..)
  , TaskSource (..)
  , SlackMessageRef (..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime, Day)
import GHC.Generics (Generic)

newtype TaskId = TaskId { unTaskId :: Text }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (FromJSON, ToJSON)

data Priority = Critical | High | Medium | Low
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data TaskStatus = Open | InProgress | Done | Archived
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data TaskSource = SlackMention SlackMessageRef | BotInteraction SlackMessageRef
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data SlackMessageRef = SlackMessageRef
  { smrChannel   :: Text
  , smrTimestamp  :: Text
  , smrUserId    :: Text
  , smrUserName  :: Maybe Text
  , smrText      :: Text
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data Task = Task
  { taskId          :: TaskId
  , taskTitle       :: Text
  , taskDescription :: Text
  , taskPriority    :: Priority
  , taskStatus      :: TaskStatus
  , taskDueDate     :: Maybe Day
  , taskEstimate    :: Maybe Text
  , taskSource      :: Maybe TaskSource
  , taskCreatedAt   :: UTCTime
  , taskUpdatedAt   :: UTCTime
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)
