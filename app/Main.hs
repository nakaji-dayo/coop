module Main where

import Coop.Config (loadConfig, Config (..), RunMode (..), ConnectionMode (..), DryrunConfig (..), SlackConfig (..), NotionConfig (..), LLMConfig (..), GoogleCalendarConfig (..), AiDelegationConfig (..), normalizeNotionId)
import Coop.Agent.CatchUp (runCatchUp)
import Coop.Agent.Core (dailyBriefing, weeklyBriefing)
import Coop.App.Env (Env (..))
import Coop.App.Log (withLogEnv, parseLogLevel)
import Coop.App.Monad (AppM, runAppM)
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
import Coop.Adapter.GitHub.TaskStore (mkGitHubTaskStoreOps)
import Control.Concurrent.STM (newTVarIO)
import qualified Data.Map.Strict as Map
import Data.Text (pack)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Katip (LogEnv)
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import qualified Network.Wai.Handler.Warp as Warp
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    AuthGoogle configPath -> do
      TIO.putStrLn $ "Loading config from: " <> pack configPath
      config <- loadConfig configPath
      manager <- newTlsManager
      runGoogleAuth (cfgGoogleCalendar config) manager
    RunDaily configPath -> runBriefing configPath "daily" dailyBriefing
    RunWeekly configPath -> runBriefing configPath "weekly" weeklyBriefing
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

data Command = AuthGoogle FilePath | RunDaily FilePath | RunWeekly FilePath | RunServer FilePath

parseArgs :: [String] -> Command
parseArgs ("auth":"google":"--config":p:_) = AuthGoogle p
parseArgs ("auth":"google":_)              = AuthGoogle "config/coop-local.dhall"
parseArgs ("briefing":"daily":"--config":p:_)  = RunDaily p
parseArgs ("briefing":"weekly":"--config":p:_) = RunWeekly p
parseArgs ("--config":p:_)                 = RunServer p
parseArgs [p]                              = RunServer p
parseArgs _                                = RunServer "config/coop-dryrun.dhall"

mkEnv :: Config -> LogEnv -> Manager -> IO (Env AppM)
mkEnv config logEnv manager = do
  case cfgMode config of
    Dryrun -> do
      taskStoreVar <- newTVarIO Map.empty
      let aiCfg = cfgAiDelegation config
      mAiStore <- case aiTaskBackend aiCfg of
        "GitHub" -> Just . mkDryrunTaskStoreOps <$> newTVarIO Map.empty
        "Notion" -> Just . mkDryrunTaskStoreOps <$> newTVarIO Map.empty
        _        -> pure Nothing
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
        , envAiTaskStore   = mAiStore
        }
    Live -> do
      let slackCfg  = cfgSlack config
          llmCfg    = cfgLLM config
          notionCfg = cfgNotion config
          gcalCfg   = cfgGoogleCalendar config
          aiCfg     = cfgAiDelegation config
          llmOps    = case llmBackend llmCfg of
            "OpenAI" -> mkOpenAILLMOps (llmOpenAI llmCfg) manager
            _        -> mkLiveLLMOps (llmClaude llmCfg) manager
          calendarOps = if T.null (googleClientId gcalCfg)
            then mkNoopCalendarStoreOps
            else mkLiveCalendarStoreOps gcalCfg manager
          aiStore = case aiTaskBackend aiCfg of
            "GitHub" -> Just $ mkGitHubTaskStoreOps aiCfg manager
            "Notion" -> Just $ mkLiveTaskStoreOps notionCfg { notionTaskDatabaseId = normalizeNotionId (aiNotionDatabaseId aiCfg) } manager
            _        -> Nothing
      pure Env
        { envConfig        = config
        , envLogEnv        = logEnv
        , envLogNamespace  = "coop"
        , envLogContext     = mempty
        , envHttpManager   = manager
        , envTaskStore     = mkLiveTaskStoreOps notionCfg manager
        , envDocStore      = mkLiveDocStoreOps (notionApiKey notionCfg) manager
        , envLLM           = llmOps
        , envNotifier      = mkLiveNotifierOps logEnv (slackBotToken slackCfg) manager
        , envCalendarStore = calendarOps
        , envAiTaskStore   = aiStore
        }

runBriefing :: FilePath -> T.Text -> AppM () -> IO ()
runBriefing configPath label action = do
  TIO.putStrLn $ "Loading config from: " <> pack configPath
  config <- loadConfig configPath
  TIO.putStrLn $ "Running " <> label <> " briefing..."
  withLogEnv (parseLogLevel (cfgLogLevel config)) $ \logEnv -> do
    manager <- newTlsManager
    env <- mkEnv config logEnv manager
    runAppM env action
  TIO.putStrLn $ label <> " briefing completed."
