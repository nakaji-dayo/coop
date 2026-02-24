module Coop.Effect.TaskStore
  ( TaskStore (..)
  ) where

import Coop.Domain.Task (Task, TaskId)

class (Monad m) => TaskStore m where
  createTask  :: Task -> m TaskId
  getTask     :: TaskId -> m (Maybe Task)
  listTasks   :: m [Task]
  updateTask  :: Task -> m ()
  archiveTask :: TaskId -> m ()
