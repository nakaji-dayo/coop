module Coop.App.Env
  ( Env (..)
  , TaskStoreOps (..)
  , DocStoreOps (..)
  , LLMOps (..)
  , NotifierOps (..)
  ) where

import Coop.Config (Config)
import Coop.Domain.Doc (DocId, Document)
import Coop.Domain.LLM (CompletionRequest, CompletionResponse)
import Coop.Domain.Notification (Notification)
import Coop.Domain.Task (Task, TaskId)
import Data.Text (Text)
import Katip (LogEnv, Namespace, LogContexts)
import Network.HTTP.Client (Manager)

data TaskStoreOps m = TaskStoreOps
  { opsCreateTask  :: Task -> m TaskId
  , opsGetTask     :: TaskId -> m (Maybe Task)
  , opsListTasks   :: m [Task]
  , opsUpdateTask  :: Task -> m ()
  , opsArchiveTask :: TaskId -> m ()
  }

data DocStoreOps m = DocStoreOps
  { opsGetDocument :: DocId -> m (Maybe Document)
  }

data LLMOps m = LLMOps
  { opsComplete :: CompletionRequest -> m CompletionResponse
  }

data NotifierOps m = NotifierOps
  { opsNotify      :: Notification -> m ()
  , opsReplyThread :: Text -> Text -> Text -> m ()
  }

data Env m = Env
  { envConfig       :: Config
  , envLogEnv       :: LogEnv
  , envLogNamespace :: Namespace
  , envLogContext   :: LogContexts
  , envHttpManager  :: Manager
  , envTaskStore    :: TaskStoreOps m
  , envDocStore     :: DocStoreOps m
  , envLLM          :: LLMOps m
  , envNotifier     :: NotifierOps m
  }
