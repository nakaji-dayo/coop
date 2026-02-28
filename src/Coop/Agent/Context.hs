module Coop.Agent.Context
  ( AgentContext (..)
  , buildContext
  , buildBriefingContext
  ) where

import Coop.App.Env (Env (..))
import Coop.App.Log (logDebug)
import Coop.Config (Config (..), NotionConfig (..))
import Coop.Domain.Calendar (CalendarEvent)
import Coop.Domain.Doc (DocId (..), Document (..))
import Coop.Domain.Task (Task (..), TaskId (..), TaskStatus (..))
import Coop.Effect.CalendarStore (CalendarStore (..))
import Coop.Effect.DocStore (DocStore (..))
import Coop.Effect.TaskStore (TaskStore (..))
import Control.Monad.Reader (MonadReader, asks)
import Data.Char (isHexDigit)
import Data.Maybe (isNothing, catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Data.Time (Day)
import Katip (KatipContext)

data AgentContext = AgentContext
  { acGuidelines      :: Text
  , acInstructions    :: Text
  , acExistingTasks   :: [Task]
  , acTaskBodies      :: Map.Map Text Text  -- ^ taskId -> body content (page + linked docs)
  , acCalendarEvents  :: [CalendarEvent]
  } deriving stock (Show)

buildContext
  :: (DocStore m, TaskStore m, MonadReader (Env m) m, KatipContext m)
  => m AgentContext
buildContext = do
  config <- asks envConfig
  let notionCfg = cfgNotion config
      guidelinesId = DocId (notionGuidelinesPageId notionCfg)
      instructionsId = DocId (notionInstructionsPageId notionCfg)

  guidelinesDoc <- getDocument guidelinesId
  instructionsDoc <- getDocument instructionsId
  tasks <- listTasks

  let guidelines = maybe "No guidelines available." docContent guidelinesDoc
      instructions = maybe "No instructions available." docContent instructionsDoc

  logDebug $ "Guidelines: " <> guidelines
  logDebug $ "Instructions: " <> instructions

  pure AgentContext
    { acGuidelines     = guidelines
    , acInstructions   = instructions
    , acExistingTasks  = tasks
    , acTaskBodies     = Map.empty
    , acCalendarEvents = []
    }

-- | Build context enriched with task body content and calendar events for briefing
buildBriefingContext
  :: (DocStore m, TaskStore m, CalendarStore m, MonadReader (Env m) m, KatipContext m)
  => Day -> m AgentContext
buildBriefingContext today = do
  baseCtx <- buildContext
  bodies <- fetchTaskBodies (acExistingTasks baseCtx)
  logDebug $ "Fetched task bodies for " <> T.pack (show (Map.size bodies)) <> " tasks"
  events <- getEvents today
  logDebug $ "Fetched " <> T.pack (show (length events)) <> " calendar events"
  pure baseCtx { acTaskBodies = bodies, acCalendarEvents = events }

-- | Fetch body content for active tasks without estimates (max 10)
fetchTaskBodies
  :: (DocStore m, KatipContext m)
  => [Task] -> m (Map.Map Text Text)
fetchTaskBodies tasks = do
  let needsBody t = isNothing (taskEstimate t)
                 && (taskStatus t == Open || taskStatus t == InProgress)
      targets = take 10 $ filter needsBody tasks
  pairs <- mapM fetchOneTaskBody targets
  pure $ Map.fromList pairs

fetchOneTaskBody
  :: (DocStore m, KatipContext m)
  => Task -> m (Text, Text)
fetchOneTaskBody task = do
  let tid = unTaskId (taskId task)
  mDoc <- getDocument (DocId tid)
  let bodyText = maybe "" docContent mDoc
  linkedTexts <- fetchLinkedDocs bodyText
  let fullContent = T.intercalate "\n---\n" (bodyText : linkedTexts)
      truncated = T.take 2000 fullContent
  logDebug $ "Task " <> tid <> " body: " <> T.take 100 truncated
  pure (tid, truncated)

-- | Fetch linked Notion documents found in text
fetchLinkedDocs :: (DocStore m) => Text -> m [Text]
fetchLinkedDocs bodyText = do
  let pageIds = extractNotionPageIds bodyText
  docs <- mapM (\pid -> getDocument (DocId pid)) (take 3 pageIds)
  pure $ map (T.take 1000 . docContent) (catMaybes docs)

-- | Extract Notion page IDs from URLs in text
extractNotionPageIds :: Text -> [Text]
extractNotionPageIds body =
  let fragments = drop 1 $ T.splitOn "notion.so/" body
  in catMaybes $ map extractIdFromFragment fragments

extractIdFromFragment :: Text -> Maybe Text
extractIdFromFragment fragment =
  let cleaned = T.takeWhile (\c -> c /= '?' && c /= '#' && c /= ' ' && c /= '\n') fragment
      segments = T.splitOn "/" cleaned
      lastSeg = if null segments then "" else last segments
      -- Page ID can be "slug-<32hex>" or just "<32hex>"
      -- Take the last 32 hex chars after removing hyphens
      stripped = T.filter (/= '-') lastSeg
      idCandidate = T.takeEnd 32 stripped
  in if T.length idCandidate == 32 && T.all isHexDigit idCandidate
     then Just idCandidate
     else Nothing
