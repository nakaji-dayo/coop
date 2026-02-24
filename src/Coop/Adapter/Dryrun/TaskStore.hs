module Coop.Adapter.Dryrun.TaskStore
  ( mkDryrunTaskStoreOps
  ) where

import Coop.App.Env (TaskStoreOps (..))
import Coop.Domain.Task (Task (..), TaskId (..), TaskStatus (..))
import Control.Concurrent.STM (TVar, atomically, readTVar, modifyTVar')
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.UUID.V4 (nextRandom)
import qualified Data.UUID as UUID
import Data.Text (pack)

mkDryrunTaskStoreOps :: (MonadIO m) => TVar (Map TaskId Task) -> TaskStoreOps m
mkDryrunTaskStoreOps store = TaskStoreOps
  { opsCreateTask = \task -> liftIO $ do
      uid <- nextRandom
      let tid = TaskId (pack (UUID.toString uid))
          task' = task { taskId = tid }
      atomically $ modifyTVar' store (Map.insert tid task')
      pure tid

  , opsGetTask = \tid -> liftIO $
      atomically $ Map.lookup tid <$> readTVar store

  , opsListTasks = liftIO $
      atomically $ Map.elems <$> readTVar store

  , opsUpdateTask = \task -> liftIO $
      atomically $ modifyTVar' store (Map.insert (taskId task) task)

  , opsArchiveTask = \tid -> liftIO $
      atomically $ modifyTVar' store (Map.adjust (\t -> t { taskStatus = Archived }) tid)
  }
