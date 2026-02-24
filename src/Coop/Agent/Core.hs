module Coop.Agent.Core
  ( handleMention
  , handleSlackEvent
  ) where

import Coop.Agent.Context (buildContext)
import Coop.Agent.Prompt (buildMentionAnalysisPrompt, parseMentionAnalysis, MentionAnalysis (..), AnalysisResult (..))
import Coop.App.Env (Env (..))
import Coop.App.Log (logInfo, logError, logWarn, logDebug)
import Coop.Config (Config (..), SlackConfig (..))
import Coop.Domain.LLM (CompletionResponse (..))
import Coop.Domain.Mention (ParsedMention (..), MentionType (..), parseMention)
import Coop.Domain.Notification (Notification (..), NotificationLevel (..))
import Coop.Domain.Task
import Coop.Effect.DocStore (DocStore)
import Coop.Effect.LLM (LLM (..))
import Coop.Effect.Notifier (Notifier (..))
import Coop.Effect.TaskStore (TaskStore (..))
import Control.Monad.Reader (MonadReader, asks)
import Data.Aeson (Value (..), Object)
import Data.Aeson.Key (fromString)
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime (getCurrentTimeZone, utcToLocalTime, localDay)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Katip (KatipContext)

-- | Process a Slack event Value. Called asynchronously after returning 200.
handleSlackEvent
  :: (TaskStore m, DocStore m, LLM m, Notifier m, MonadReader (Env m) m, MonadIO m, KatipContext m)
  => Value -> m ()
handleSlackEvent (Object obj) = do
  case KM.lookup "event" obj of
    Just (Object event) -> processEvent event
    _ -> logWarn "event_callback without event object"
handleSlackEvent _ = pure ()

processEvent
  :: (TaskStore m, DocStore m, LLM m, Notifier m, MonadReader (Env m) m, MonadIO m, KatipContext m)
  => Object -> m ()
processEvent event = do
  config <- asks envConfig
  let slackCfg        = cfgSlack config
      botUserId       = slackBotUserId slackCfg
      monitoredUserId = slackMonitoredUserId slackCfg

  -- Extract event fields
  let mType    = lookupText "type" event
      mChannel = lookupText "channel" event
      mTs      = lookupText "ts" event
      mUser    = lookupText "user" event
      mText    = lookupText "text" event
      mSubtype = lookupText "subtype" event

  logDebug $ "processEvent type=" <> T.pack (show mType) <> " subtype=" <> T.pack (show mSubtype)
    <> " user=" <> T.pack (show mUser) <> " channel=" <> T.pack (show mChannel)
  logDebug $ "processEvent text=" <> T.pack (show mText)
  logDebug $ "processEvent botUserId=" <> botUserId <> " monitoredUserId=" <> monitoredUserId

  case (mType, mChannel, mTs, mUser, mText) of
    (Just "message", Just channel, Just ts, Just user, Just text)
      | mSubtype == Nothing
      , user /= botUserId
      -> do
          let mMention = parseMention monitoredUserId channel ts user Nothing text
          logDebug $ "processEvent parseMention result=" <> T.pack (show (fmap pmMentionType mMention))
          case mMention of
            Just mention -> handleMention mention
            Nothing -> logDebug "processEvent monitored user not mentioned, skipping"
    _ -> logDebug "processEvent event did not match message pattern, skipping"

lookupText :: Text -> Object -> Maybe Text
lookupText key obj = case KM.lookup (fromString $ T.unpack key) obj of
  Just (String t) -> Just t
  _               -> Nothing

-- | Handle a parsed mention: analyze, create task, notify.
handleMention
  :: (TaskStore m, DocStore m, LLM m, Notifier m, MonadReader (Env m) m, MonadIO m, KatipContext m)
  => ParsedMention -> m ()
handleMention mention = do
  config <- asks envConfig
  let notifyChannel = slackNotifyChannel (cfgSlack config)

  logInfo $ "Processing mention from " <> pmUserId mention <> " in " <> pmChannel mention

  ctx <- buildContext

  case pmMentionType mention of
    BotCommand -> do
      -- Analyze the mention and create a task
      now0 <- liftIO getCurrentTime
      tz <- liftIO getCurrentTimeZone
      let today = localDay (utcToLocalTime tz now0)
          prompt = buildMentionAnalysisPrompt ctx today mention
      resp <- complete prompt
      logInfo $ "LLM analysis response: " <> T.take 200 (crResponseText resp)

      case parseMentionAnalysis (crResponseText resp) of
        Left err -> do
          logError $ "Failed to parse analysis: " <> T.pack err
          -- Fallback: create task with raw text, still notify
          now <- liftIO getCurrentTime
          let task = Task
                { taskId = TaskId ""
                , taskTitle = T.take 100 (pmStrippedText mention)
                , taskDescription = crResponseText resp
                , taskPriority = Medium
                , taskStatus = Open
                , taskDueDate = Nothing
                , taskSource = SlackMention (toSlackRef mention)
                , taskCreatedAt = now
                , taskUpdatedAt = now
                }
          tid <- createTask task
          logInfo $ "Created task (fallback): " <> unTaskId tid

          replyThread (pmChannel mention) (pmTimestamp mention) $
            "Task created: " <> T.take 100 (pmStrippedText mention) <> " [Medium]"
          notify Notification
            { notifChannel = notifyChannel
            , notifText = formatNotification mention tid Medium (T.take 100 (pmStrippedText mention)) "Could not parse LLM analysis"
            , notifLevel = Info
            }

        Right (SkipTask reason) -> do
          logInfo $ "Skipped task creation: " <> reason

        Right (CreateTask analysis) -> do
          now <- liftIO getCurrentTime
          let task = Task
                { taskId = TaskId ""
                , taskTitle = maTitle analysis
                , taskDescription = maDescription analysis
                , taskPriority = maPriority analysis
                , taskStatus = Open
                , taskDueDate = maDueDate analysis
                , taskSource = SlackMention (toSlackRef mention)
                , taskCreatedAt = now
                , taskUpdatedAt = now
                }
          tid <- createTask task
          logInfo $ "Created task: " <> unTaskId tid <> " [" <> T.pack (show (maPriority analysis)) <> "]"

          replyThread (pmChannel mention) (pmTimestamp mention) $
            "Task created: " <> maTitle analysis <> " [" <> T.pack (show (maPriority analysis)) <> "]\n"
            <> "Reason: " <> maReason analysis
          notify Notification
            { notifChannel = notifyChannel
            , notifText = formatNotification mention tid (maPriority analysis) (maTitle analysis) (maReason analysis)
            , notifLevel = priorityToLevel (maPriority analysis)
            }

    DirectMention -> do
      -- Just a mention without content - acknowledge
      replyThread (pmChannel mention) (pmTimestamp mention)
        "Hi! You mentioned me but didn't include a message. Try mentioning me with a task description."

toSlackRef :: ParsedMention -> SlackMessageRef
toSlackRef pm = SlackMessageRef
  { smrChannel  = pmChannel pm
  , smrTimestamp = pmTimestamp pm
  , smrUserId   = pmUserId pm
  , smrUserName = pmUserName pm
  , smrText     = pmText pm
  }

formatNotification :: ParsedMention -> TaskId -> Priority -> Text -> Text -> Text
formatNotification mention tid prio title reason = T.unlines
  [ priorityEmoji prio <> " *New Task* [" <> priorityLabel prio <> "]"
  , ""
  , "*" <> title <> "*"
  , ""
  , "Requested by: <@" <> pmUserId mention <> ">"
  , "Reason: " <> reason
  , ""
  , "Notion: " <> notionPageUrl tid
  , "Slack: " <> slackMessageUrl mention
  ]

notionPageUrl :: TaskId -> Text
notionPageUrl tid = "https://www.notion.so/" <> T.replace "-" "" (unTaskId tid)

slackMessageUrl :: ParsedMention -> Text
slackMessageUrl mention =
  let ts = T.replace "." "" (pmTimestamp mention)
  in "https://slack.com/archives/" <> pmChannel mention <> "/p" <> ts

priorityEmoji :: Priority -> Text
priorityEmoji Critical = ":rotating_light:"
priorityEmoji High     = ":warning:"
priorityEmoji Medium   = ":blue_book:"
priorityEmoji Low      = ":memo:"

priorityLabel :: Priority -> Text
priorityLabel Critical = "CRITICAL"
priorityLabel High     = "HIGH"
priorityLabel Medium   = "MEDIUM"
priorityLabel Low      = "LOW"

priorityToLevel :: Priority -> NotificationLevel
priorityToLevel Critical = Urgent
priorityToLevel High     = Warning
priorityToLevel Medium   = Info
priorityToLevel Low      = Info
