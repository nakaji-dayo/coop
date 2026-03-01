module Coop.Agent.Core
  ( handleMention
  , handleSlackEvent
  , processEvent
  , dailyBriefing
  ) where

import Coop.Agent.Context (buildContext, buildBriefingContext)
import Coop.Agent.Prompt (buildMentionAnalysisPrompt, buildDailyBriefingPrompt, parseMentionAnalysis, parseDailyBriefing, MentionAnalysis (..), AnalysisResult (..), DailyBriefing (..), BriefingTask (..), EstimateRequest (..), MeetingPrep (..))
import Coop.App.Env (Env (..))
import Coop.App.Log (logInfo, logError, logWarn, logDebug)
import Coop.Config (Config (..), SlackConfig (..))
import Coop.Domain.LLM (CompletionResponse (..))
import Coop.Domain.Mention (ParsedMention (..), MentionType (..), parseMention)
import Coop.Domain.Notification (Notification (..), NotificationLevel (..))
import Coop.Domain.Task
import Coop.Effect.CalendarStore (CalendarStore)
import Coop.Effect.DocStore (DocStore)
import Coop.Effect.LLM (LLM (..))
import Coop.Effect.Notifier (Notifier (..))
import Coop.Effect.TaskStore (TaskStore (..))
import Control.Monad.Reader (MonadReader, asks)
import Data.Aeson (Value (..), Object)
import Data.Aeson.Key (fromString)
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime (getCurrentTimeZone, utcToLocalTime, localDay)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Katip (KatipContext)

-- | Process a Slack event Value. Called asynchronously after returning 200.
handleSlackEvent
  :: (TaskStore m, DocStore m, LLM m, Notifier m, CalendarStore m, MonadReader (Env m) m, MonadIO m, KatipContext m)
  => Value -> m ()
handleSlackEvent (Object obj) = do
  case KM.lookup "event" obj of
    Just (Object event) -> processEvent event
    _ -> logWarn "event_callback without event object"
handleSlackEvent _ = pure ()

processEvent
  :: (TaskStore m, DocStore m, LLM m, Notifier m, CalendarStore m, MonadReader (Env m) m, MonadIO m, KatipContext m)
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
  :: (TaskStore m, DocStore m, LLM m, Notifier m, CalendarStore m, MonadReader (Env m) m, MonadIO m, KatipContext m)
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
                , taskEstimate = Nothing
                , taskSource = Just (SlackMention (toSlackRef mention))
                , taskCreatedAt = now
                , taskUpdatedAt = now
                }
          tid <- createTask task
          logInfo $ "Created task (fallback): " <> unTaskId tid

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
                , taskEstimate = Nothing
                , taskSource = Just (SlackMention (toSlackRef mention))
                , taskCreatedAt = now
                , taskUpdatedAt = now
                }
          tid <- createTask task
          logInfo $ "Created task: " <> unTaskId tid <> " [" <> T.pack (show (maPriority analysis)) <> "]"

          notify Notification
            { notifChannel = notifyChannel
            , notifText = formatNotification mention tid (maPriority analysis) (maTitle analysis) (maReason analysis)
            , notifLevel = priorityToLevel (maPriority analysis)
            }

    DirectMention -> do
      logDebug "Direct mention without content, skipping"

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

-- | Run the daily briefing pipeline
dailyBriefing
  :: (TaskStore m, DocStore m, LLM m, Notifier m, CalendarStore m, MonadReader (Env m) m, MonadIO m, KatipContext m)
  => m ()
dailyBriefing = do
  config <- asks envConfig
  let notifyChannel = slackNotifyChannel (cfgSlack config)

  logInfo "Starting daily briefing"

  now0 <- liftIO getCurrentTime
  tz <- liftIO getCurrentTimeZone
  let today = localDay (utcToLocalTime tz now0)

  ctx <- buildBriefingContext today

  let prompt = buildDailyBriefingPrompt ctx today tz

  resp <- complete prompt
  logInfo $ "Daily briefing LLM response: " <> T.take 200 (crResponseText resp)

  let briefingText = case parseDailyBriefing (crResponseText resp) of
        Left err -> do
          T.unlines
            [ ":sunrise: *Daily Briefing*"
            , ""
            , crResponseText resp
            , ""
            , "_(" <> T.pack err <> ")_"
            ]
        Right briefing -> formatDailyBriefing briefing

  notify Notification
    { notifChannel = notifyChannel
    , notifText = briefingText
    , notifLevel = Info
    }

  logInfo "Daily briefing completed"

formatDailyBriefing :: DailyBriefing -> Text
formatDailyBriefing briefing = T.unlines $ concat
  [ [ ":sunrise: *Daily Briefing*"
    , ""
    , dbSummary briefing
    , ""
    , "*Today's Schedule:*"
    ]
  , if null (dbSchedule briefing)
    then ["No tasks scheduled for today."]
    else map formatBriefingTask (dbSchedule briefing)
         <> [totalHoursLine (dbMeetingHours briefing) (dbSchedule briefing)]
  , if null (dbMeetingPreps briefing) then []
    else
      [ ""
      , "*Meeting Prep:*"
      ] <> map formatMeetingPrep (dbMeetingPreps briefing)
  , if null (dbEstimateRequests briefing) then []
    else
      [ ""
      , "*Estimate Requests:*"
      ] <> map formatEstimateRequest (dbEstimateRequests briefing)
  ]

formatBriefingTask :: BriefingTask -> Text
formatBriefingTask bt = T.concat
  [ if btIsMust bt then ":red_circle: " else ":white_circle: "
  , "[" <> btPriority bt <> "] "
  , "<" <> notionPageUrl (TaskId (btTaskId bt)) <> "|" <> btTitle bt <> ">"
  , case btEstimateHours bt of
      Just h  -> " (" <> formatHours h <> ")"
      Nothing -> ""
  , " — " <> btReason bt
  ]

totalHoursLine :: Double -> [BriefingTask] -> Text
totalHoursLine mtgHours tasks =
  let total = sum $ map (fromMaybe 0 . btEstimateHours) tasks
      available = max 0 (8.0 - mtgHours)
  in "\n:clock3: *Total: " <> formatHours total <> " / " <> formatHours available <> " available* (8h - " <> formatHours mtgHours <> " meetings)"

formatHours :: Double -> Text
formatHours h
  | h < 1     = T.pack (show (round (h * 60) :: Int)) <> "min"
  | h == fromIntegral (round h :: Int) = T.pack (show (round h :: Int)) <> "h"
  | otherwise = T.pack (show h) <> "h"

formatMeetingPrep :: MeetingPrep -> Text
formatMeetingPrep mp = T.concat
  [ ":clipboard: "
  , "*" <> mpTitle mp <> "*"
  , " — " <> mpReason mp
  ]

formatEstimateRequest :: EstimateRequest -> Text
formatEstimateRequest er = T.concat
  [ ":hourglass: "
  , "<" <> notionPageUrl (TaskId (erTaskId er)) <> "|" <> erTitle er <> ">"
  , " — " <> erReason er
  ]
