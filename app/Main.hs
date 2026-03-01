module Main where

import Coop.Config (loadConfig, Config (..), RunMode (..), ConnectionMode (..), DryrunConfig (..), SlackConfig (..), NotionConfig (..), LLMConfig (..), GoogleCalendarConfig (..))
import Coop.Agent.CatchUp (runCatchUp)
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
import Coop.Adapter.Dryrun.CalendarStore (mkDryrunCalendarStoreOps)
import Coop.Adapter.Claude.LLM (mkLiveLLMOps)
import Coop.Adapter.OpenAI.LLM (mkOpenAILLMOps)
import Coop.Adapter.Notion.DocStore (mkLiveDocStoreOps)
import Coop.Adapter.Notion.TaskStore (mkLiveTaskStoreOps)
import Coop.Adapter.Slack.Notifier (mkLiveNotifierOps)
import Coop.Adapter.GoogleCalendar.Auth (runGoogleAuth)
import Coop.Adapter.GoogleCalendar.CalendarStore (mkLiveCalendarStoreOps, mkNoopCalendarStoreOps)
import Control.Concurrent.STM (newTVarIO)
import qualified Data.Map.Strict as Map
import Data.Text (pack)
import qualified Data.Text as T
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
  case parseArgs args of
    AuthGoogle configPath -> do
      TIO.putStrLn $ "Loading config from: " <> pack configPath
      config <- loadConfig configPath
      manager <- newTlsManager
      runGoogleAuth (cfgGoogleCalendar config) manager
    RunServer configPath -> do
      TIO.putStrLn $ "Loading config from: " <> pack configPath
      config <- loadConfig configPath
      TIO.putStrLn $ "Mode: " <> pack (show (cfgMode config))
      TIO.putStrLn $ "Log level: " <> cfgLogLevel config

      withLogEnv (parseLogLevel (cfgLogLevel config)) $ \logEnv -> do
        manager <- newTlsManager
        env <- mkEnv config logEnv manager
        runCatchUp env
        let port = fromIntegral (cfgPort config)
        TIO.putStrLn $ "Starting coop on port " <> pack (show port)
        case slackConnectionMode (cfgSlack config) of
          Webhook ->
            race_ (Warp.run port (mkApp env)) (runScheduler env)
          SocketMode -> do
            TIO.putStrLn "Using Socket Mode for Slack connection"
            race_ (race_ (Warp.run port (mkHealthApp env)) (runSlackSocketMode env)) (runScheduler env)

data Command = AuthGoogle FilePath | RunServer FilePath

parseArgs :: [String] -> Command
parseArgs ("auth":"google":"--config":p:_) = AuthGoogle p
parseArgs ("auth":"google":_)              = AuthGoogle "config/coop.dhall"
parseArgs ("--config":p:_)                 = RunServer p
parseArgs [p]                              = RunServer p
parseArgs _                                = RunServer "config/coop-dryrun.dhall"

mkEnv :: Config -> LogEnv -> Manager -> IO (Env AppM)
mkEnv config logEnv manager = do
  case cfgMode config of
    Dryrun -> do
      taskStoreVar <- newTVarIO Map.empty
      pure Env
        { envConfig        = config
        , envLogEnv        = logEnv
        , envLogNamespace  = "coop"
        , envLogContext     = mempty
        , envHttpManager   = manager
        , envTaskStore     = mkDryrunTaskStoreOps taskStoreVar
        , envDocStore      = mkDryrunDocStoreOps (dryrunDataDir (cfgDryrun config))
        , envLLM           = mkDryrunLLMOps
        , envNotifier      = mkDryrunNotifierOps
        , envCalendarStore = mkDryrunCalendarStoreOps
        }
    Live -> do
      let slackCfg  = cfgSlack config
          llmCfg    = cfgLLM config
          notionCfg = cfgNotion config
          gcalCfg   = cfgGoogleCalendar config
          llmOps    = case llmBackend llmCfg of
            "OpenAI" -> mkOpenAILLMOps (llmOpenAI llmCfg) manager
            _        -> mkLiveLLMOps (llmClaude llmCfg) manager
          calendarOps = if T.null (googleClientId gcalCfg)
            then mkNoopCalendarStoreOps
            else mkLiveCalendarStoreOps gcalCfg manager
      pure Env
        { envConfig        = config
        , envLogEnv        = logEnv
        , envLogNamespace  = "coop"
        , envLogContext     = mempty
        , envHttpManager   = manager
        , envTaskStore     = mkLiveTaskStoreOps notionCfg manager
        , envDocStore      = mkLiveDocStoreOps (notionApiKey notionCfg) manager
        , envLLM           = llmOps
        , envNotifier      = mkLiveNotifierOps (slackBotToken slackCfg) manager
        , envCalendarStore = calendarOps
        }
