module Main where

import Coop.Config (loadConfig, Config (..), RunMode (..), ConnectionMode (..), DryrunConfig (..), SlackConfig (..), NotionConfig (..))
import Coop.App.Env (Env (..))
import Coop.App.Log (withLogEnv, parseLogLevel)
import Coop.App.Monad (AppM)
import Coop.Server.Handlers (mkApp, mkHealthApp)
import Coop.Adapter.Slack.SocketMode (runSlackSocketMode)
import Coop.Scheduler (runScheduler)
import UnliftIO.Async (race_)
import Coop.Adapter.Dryrun.TaskStore (mkDryrunTaskStoreOps)
import Coop.Adapter.Dryrun.DocStore (mkDryrunDocStoreOps)
import Coop.Adapter.Dryrun.LLM (mkDryrunLLMOps)
import Coop.Adapter.Dryrun.Notifier (mkDryrunNotifierOps)
import Coop.Adapter.Claude.LLM (mkLiveLLMOps)
import Coop.Adapter.Notion.DocStore (mkLiveDocStoreOps)
import Coop.Adapter.Notion.TaskStore (mkLiveTaskStoreOps)
import Coop.Adapter.Slack.Notifier (mkLiveNotifierOps)
import Control.Concurrent.STM (newTVarIO)
import qualified Data.Map.Strict as Map
import Data.Text (pack)
import qualified Data.Text.IO as TIO
import Katip (LogEnv)
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import qualified Network.Wai.Handler.Warp as Warp
import Configuration.Dotenv (loadFile, defaultConfig, onMissingFile)
import System.Environment (getArgs)

main :: IO ()
main = do
  loadFile defaultConfig `onMissingFile` pure ()
  args <- getArgs
  let configPath = case args of
        ["--config", p] -> p
        [p]             -> p
        _               -> "config/coop-dryrun.dhall"

  TIO.putStrLn $ "Loading config from: " <> pack configPath
  config <- loadConfig configPath
  TIO.putStrLn $ "Mode: " <> pack (show (cfgMode config))
  TIO.putStrLn $ "Log level: " <> cfgLogLevel config

  withLogEnv (parseLogLevel (cfgLogLevel config)) $ \logEnv -> do
    manager <- newTlsManager
    env <- mkEnv config logEnv manager
    let port = fromIntegral (cfgPort config)
    TIO.putStrLn $ "Starting coop on port " <> pack (show port)
    case slackConnectionMode (cfgSlack config) of
      Webhook ->
        race_ (Warp.run port (mkApp env)) (runScheduler env)
      SocketMode -> do
        TIO.putStrLn "Using Socket Mode for Slack connection"
        race_ (race_ (Warp.run port (mkHealthApp env)) (runSlackSocketMode env)) (runScheduler env)

mkEnv :: Config -> LogEnv -> Manager -> IO (Env AppM)
mkEnv config logEnv manager = do
  case cfgMode config of
    Dryrun -> do
      taskStoreVar <- newTVarIO Map.empty
      pure Env
        { envConfig       = config
        , envLogEnv       = logEnv
        , envLogNamespace = "coop"
        , envLogContext    = mempty
        , envHttpManager  = manager
        , envTaskStore    = mkDryrunTaskStoreOps taskStoreVar
        , envDocStore     = mkDryrunDocStoreOps (dryrunDataDir (cfgDryrun config))
        , envLLM          = mkDryrunLLMOps
        , envNotifier     = mkDryrunNotifierOps
        }
    Live -> do
      let slackCfg  = cfgSlack config
          claudeCfg = cfgClaude config
          notionCfg = cfgNotion config
      pure Env
        { envConfig       = config
        , envLogEnv       = logEnv
        , envLogNamespace = "coop"
        , envLogContext    = mempty
        , envHttpManager  = manager
        , envTaskStore    = mkLiveTaskStoreOps notionCfg manager
        , envDocStore     = mkLiveDocStoreOps (notionApiKey notionCfg) manager
        , envLLM          = mkLiveLLMOps claudeCfg manager
        , envNotifier     = mkLiveNotifierOps (slackBotToken slackCfg) manager
        }
