module Coop.Adapter.GitHub.TaskStore
  ( mkGitHubTaskStoreOps
  ) where

import Coop.Adapter.GitHub.Client (githubGet, githubPost, githubPatch)
import Coop.Adapter.GitHub.Types (GitHubIssue (..), GitHubCreateIssueRequest (..), GitHubUpdateIssueRequest (..))
import Coop.App.Env (TaskStoreOps (..))
import Coop.Config (AiDelegationConfig (..))
import Coop.Domain.Task (Task (..), TaskId (..), Priority (..), TaskStatus (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Value)
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Data.Time (UTCTime (..))
import Data.Time.Calendar (fromGregorian)
import Network.HTTP.Client (Manager)

mkGitHubTaskStoreOps :: (MonadIO m) => AiDelegationConfig -> Manager -> TaskStoreOps m
mkGitHubTaskStoreOps cfg manager =
  let token = aiGitHubToken cfg
      repo  = aiGitHubRepo cfg   -- "owner/repo"
      label = aiGitHubLabel cfg
      baseUrl = "https://api.github.com/repos/" <> unpack repo
  in TaskStoreOps
  { opsCreateTask = \task -> liftIO $ do
      let req = GitHubCreateIssueRequest
            { gcrTitle  = taskTitle task
            , gcrBody   = taskDescription task
            , gcrLabels = if T.null label then [] else [label]
            }
      result <- githubPost manager token (baseUrl <> "/issues") req
      case result of
        Left err -> error $ "Failed to create GitHub issue: " <> err
        Right (issue :: GitHubIssue) -> pure $ TaskId (T.pack (show (ghiNumber issue)))

  , opsGetTask = \_tid -> liftIO $ pure Nothing

  , opsListTasks = liftIO $ do
      let url = baseUrl <> "/issues?labels=" <> unpack label <> "&state=open"
      result <- githubGet manager token url
      case result of
        Left _err -> pure []
        Right issues -> pure $ map (issueToTask repo) issues

  , opsUpdateTask = \_task -> liftIO $ pure ()

  , opsArchiveTask = \tid -> liftIO $ do
      let url = baseUrl <> "/issues/" <> unpack (unTaskId tid)
          req = GitHubUpdateIssueRequest { gurState = "closed" }
      _ <- githubPatch manager token url req :: IO (Either String Value)
      pure ()
  }

issueToTask :: Text -> GitHubIssue -> Task
issueToTask _repo issue =
  let epoch = UTCTime (fromGregorian 2000 1 1) 0
  in Task
    { taskId          = TaskId (T.pack (show (ghiNumber issue)))
    , taskTitle       = ghiTitle issue
    , taskDescription = maybe "" id (ghiBody issue)
    , taskPriority    = Medium
    , taskStatus      = if ghiState issue == "open" then Open else Done
    , taskDueDate     = Nothing
    , taskEstimate    = Nothing
    , taskSource      = Nothing
    , taskCreatedAt   = epoch
    , taskUpdatedAt   = epoch
    }
