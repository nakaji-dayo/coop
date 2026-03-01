module Coop.Agent.CatchUpSpec (spec) where

import Test.Hspec

import Coop.Agent.CatchUp (CatchUpDeps (..), parseChannels, runCatchUpWith)
import Coop.App.Env (Env (..))
import Coop.App.Monad (AppM)
import Coop.Config
import Coop.Adapter.Dryrun.TaskStore (mkDryrunTaskStoreOps)
import Coop.Adapter.Dryrun.DocStore (mkDryrunDocStoreOps)
import Coop.Adapter.Dryrun.LLM (mkDryrunLLMOps)
import Coop.Adapter.Dryrun.Notifier (mkDryrunNotifierOps)
import Coop.Adapter.Dryrun.CalendarStore (mkDryrunCalendarStoreOps)
import Coop.App.Log (withLogEnv)
import Control.Concurrent.STM (newTVarIO)
import Data.Aeson (Object, Value (..))
import Data.Aeson.Key (fromString)
import qualified Data.Aeson.KeyMap as KM
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Katip (Severity(..))
import Network.HTTP.Client.TLS (newTlsManager)

spec :: Spec
spec = do
  describe "parseChannels" $ do
    it "returns empty list for empty string" $
      parseChannels "" `shouldBe` []

    it "parses single channel" $
      parseChannels "C123" `shouldBe` ["C123"]

    it "parses multiple channels" $
      parseChannels "C123,C456,C789" `shouldBe` ["C123", "C456", "C789"]

    it "trims whitespace" $
      parseChannels " C123 , C456 " `shouldBe` ["C123", "C456"]

    it "filters empty segments" $
      parseChannels "C123,,C456," `shouldBe` ["C123", "C456"]

  describe "runCatchUpWith" $ do
    it "skips when no channels configured" $ do
      tsRef <- newIORef Nothing
      env <- mkTestEnv ""
      let deps = mkMockDeps (const $ const $ pure (Right [])) tsRef (Just "1000.0")
      runCatchUpWith deps env
      -- No ts operations since there are no channels to process
      readIORef tsRef `shouldReturn` Nothing

    it "records current ts on first run and skips" $ do
      tsRef <- newIORef Nothing
      env <- mkTestEnv "C123"
      let deps = mkMockDeps (const $ const $ pure (Right [])) tsRef Nothing
      runCatchUpWith deps env
      readIORef tsRef `shouldReturn` Just "9999.0"

    it "processes messages and updates ts" $ do
      tsRef <- newIORef Nothing
      env <- mkTestEnv "C123"
      let msgs = [mkMsg "1001.0" "U_USER" "hello <@U_MONITORED> do something"]
          deps = mkMockDeps (const $ const $ pure (Right msgs)) tsRef (Just "1000.0")
      runCatchUpWith deps env
      readIORef tsRef `shouldReturn` Just "1001.0"

    it "processes messages from multiple channels" $ do
      tsRef <- newIORef Nothing
      processedRef <- newIORef ([] :: [Text])
      env <- mkTestEnv "C111,C222"
      let fetchMock chanId _oldest = do
            modifyIORef processedRef (<> [chanId])
            pure $ Right [mkMsg "2000.0" "U_USER" "hello <@U_MONITORED> task"]
          deps = mkMockDeps fetchMock tsRef (Just "1000.0")
      runCatchUpWith deps env
      readIORef processedRef `shouldReturn` ["C111", "C222"]

    it "continues on fetch failure" $ do
      tsRef <- newIORef Nothing
      env <- mkTestEnv "C_FAIL,C_OK"
      let fetchMock chanId _oldest
            | chanId == "C_FAIL" = pure $ Left "Slack API error: channel_not_found"
            | otherwise = pure $ Right [mkMsg "3000.0" "U_USER" "hello <@U_MONITORED> task"]
          deps = mkMockDeps fetchMock tsRef (Just "1000.0")
      runCatchUpWith deps env
      -- ts should be updated from the successful channel
      readIORef tsRef `shouldReturn` Just "3000.0"

    it "skips individual message processing failures" $ do
      tsRef <- newIORef Nothing
      env <- mkTestEnv "C123"
      -- First message has no "type" field so processEvent skips it (no crash),
      -- second message is valid
      let msgs = [ mkMsgRaw "2000.0"  -- no type/user/text fields → processEvent skips
                 , mkMsg "2001.0" "U_USER" "hello <@U_MONITORED> do another thing"
                 ]
          deps = mkMockDeps (const $ const $ pure (Right msgs)) tsRef (Just "1000.0")
      runCatchUpWith deps env
      -- Both messages processed without crash, ts updated to latest
      readIORef tsRef `shouldReturn` Just "2001.0"

-- Helpers

mkMsg :: Text -> Text -> Text -> Object
mkMsg ts user text = KM.fromList
  [ (fromString "type", String "message")
  , (fromString "ts", String ts)
  , (fromString "user", String user)
  , (fromString "text", String text)
  ]

mkMsgRaw :: Text -> Object
mkMsgRaw ts = KM.fromList
  [ (fromString "ts", String ts)
  ]

mkMockDeps
  :: (Text -> Maybe Text -> IO (Either Text [Object]))
  -> IORef (Maybe Text)
  -> Maybe Text       -- ^ initial last-ts value
  -> CatchUpDeps
mkMockDeps fetchFn tsRef initialTs =
  let firstRead = newIORef True
  in CatchUpDeps
    { depFetchHistory = fetchFn
    , depReadLastTs   = do
        ref <- firstRead
        isFirst <- readIORef ref
        if isFirst
          then do
            writeIORef ref False
            pure initialTs
          else readIORef tsRef
    , depWriteLastTs  = \ts -> writeIORef tsRef (Just ts)
    , depCurrentTs    = pure "9999.0"
    }

mkTestEnv :: Text -> IO (Env AppM)
mkTestEnv catchupChannels = do
  taskStoreVar <- newTVarIO Map.empty
  manager <- newTlsManager
  withLogEnv ErrorS $ \logEnv ->
    pure Env
      { envConfig = testConfig catchupChannels
      , envLogEnv = logEnv
      , envLogNamespace = "test"
      , envLogContext = mempty
      , envHttpManager = manager
      , envTaskStore = mkDryrunTaskStoreOps taskStoreVar
      , envDocStore = mkDryrunDocStoreOps "config/dryrun-data"
      , envLLM = mkDryrunLLMOps
      , envNotifier = mkDryrunNotifierOps
      , envCalendarStore = mkDryrunCalendarStoreOps
      }

testConfig :: Text -> Config
testConfig catchupChannels = Config
  { cfgMode = Live
  , cfgPort = 3000
  , cfgLogLevel = "ERROR"
  , cfgSlack = SlackConfig
      { slackBotToken = "xoxb-test"
      , slackSigningSecret = "test-secret"
      , slackBotUserId = "U_BOT"
      , slackMonitoredUserId = "U_MONITORED"
      , slackNotifyChannel = "C_NOTIFY"
      , slackAppToken = ""
      , slackConnectionMode = Webhook
      , slackCatchupChannels = catchupChannels
      }
  , cfgLLM = LLMConfig
      { llmBackend = "Claude"
      , llmClaude = ClaudeConfig { claudeApiKey = "test", claudeModel = "test" }
      , llmOpenAI = OpenAIConfig { openaiApiKey = "test", openaiModel = "test" }
      }
  , cfgNotion = NotionConfig
      { notionApiKey = "test"
      , notionTaskDatabaseId = "test"
      , notionGuidelinesPageId = "test"
      , notionInstructionsPageId = "test"
      , notionPropName = "Name"
      , notionPropPriority = "Priority"
      , notionPropStatus = "Status"
      , notionPropDueDate = ""
      , notionStatusOpen = "Open"
      , notionStatusInProgress = "In Progress"
      , notionStatusDone = "Done"
      , notionPropAssignee = ""
      , notionAssigneeUserId = ""
      , notionPropEstimate = ""
      }
  , cfgDryrun = DryrunConfig { dryrunDataDir = "config/dryrun-data" }
  , cfgScheduler = SchedulerConfig { schedulerBriefingCron = "-", schedulerWeeklyBriefingCron = "-", schedulerWeeklyAvailableHours = 30 }
  , cfgGoogleCalendar = GoogleCalendarConfig
      { googleClientId = ""
      , googleClientSecret = ""
      , googleCalendarId = ""
      , googleTokenPath = ""
      }
  }
