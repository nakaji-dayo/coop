module Coop.App.Monad
  ( AppM (..)
  , runAppM
  ) where

import Coop.App.Env (Env (..))
import qualified Coop.App.Env as Env
import Coop.Effect.CalendarStore (CalendarStore (..))
import Coop.Effect.TaskStore (TaskStore (..))
import Coop.Effect.DocStore (DocStore (..))
import Coop.Effect.LLM (LLM (..))
import Coop.Effect.Notifier (Notifier (..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, ask, asks, local)
import Katip

newtype AppM a = AppM { unAppM :: ReaderT (Env AppM) IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader (Env AppM))

instance MonadFail AppM where
  fail = AppM . fail

runAppM :: Env AppM -> AppM a -> IO a
runAppM env (AppM m) = runReaderT m env

instance Katip AppM where
  getLogEnv = asks envLogEnv
  localLogEnv f (AppM m) = AppM $ do
    env <- ask
    let env' = env { envLogEnv = f (envLogEnv env) }
    local (const env') m

instance KatipContext AppM where
  getKatipContext = asks envLogContext
  localKatipContext f (AppM m) = AppM $ do
    env <- ask
    let env' = env { envLogContext = f (envLogContext env) }
    local (const env') m
  getKatipNamespace = asks envLogNamespace
  localKatipNamespace f (AppM m) = AppM $ do
    env <- ask
    let env' = env { envLogNamespace = f (envLogNamespace env) }
    local (const env') m

instance TaskStore AppM where
  createTask t  = do ops <- asks envTaskStore; Env.opsCreateTask ops t
  getTask tid   = do ops <- asks envTaskStore; Env.opsGetTask ops tid
  listTasks     = do ops <- asks envTaskStore; Env.opsListTasks ops
  updateTask t  = do ops <- asks envTaskStore; Env.opsUpdateTask ops t
  archiveTask t = do ops <- asks envTaskStore; Env.opsArchiveTask ops t

instance DocStore AppM where
  getDocument did = do ops <- asks envDocStore; Env.opsGetDocument ops did

instance LLM AppM where
  complete req = do ops <- asks envLLM; Env.opsComplete ops req

instance Notifier AppM where
  notify n = do ops <- asks envNotifier; Env.opsNotify ops n
  replyThread c ts msg = do ops <- asks envNotifier; Env.opsReplyThread ops c ts msg

instance CalendarStore AppM where
  getEvents day = do ops <- asks envCalendarStore; Env.opsGetEvents ops day
