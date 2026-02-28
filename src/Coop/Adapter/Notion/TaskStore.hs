module Coop.Adapter.Notion.TaskStore
  ( mkLiveTaskStoreOps
  ) where

import Coop.Adapter.Notion.Client (notionPost, notionPatch)
import Coop.Adapter.Notion.Types (NotionPage (..), NotionQueryResponse (..), NotionCreatePageRequest (..), NotionDatabaseQuery (..))
import Coop.App.Env (TaskStoreOps (..))
import Coop.Config (NotionConfig (..))
import Coop.Domain.Task (Task (..), TaskId (..), TaskStatus (..), Priority (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.Key (fromText)
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Data.Time (Day, UTCTime (..))
import Data.Time.Calendar (fromGregorian)
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Network.HTTP.Client (Manager)

mkLiveTaskStoreOps :: (MonadIO m) => NotionConfig -> Manager -> TaskStoreOps m
mkLiveTaskStoreOps notionCfg manager =
  let apiKey     = notionApiKey notionCfg
      databaseId = notionTaskDatabaseId notionCfg
  in TaskStoreOps
  { opsCreateTask = \task -> liftIO $ do
      let props = taskToNotionProperties notionCfg task
          children = taskToNotionBody task
          req = NotionCreatePageRequest databaseId props children
      result <- notionPost manager apiKey "https://api.notion.com/v1/pages" req
      case result of
        Left err -> error $ "Failed to create Notion task: " <> err
        Right (page :: NotionPage) -> pure $ TaskId (npgId page)

  , opsGetTask = \_tid -> liftIO $ do
      -- Simplified: would need to query by ID and convert back
      pure Nothing

  , opsListTasks = liftIO $ do
      let url = "https://api.notion.com/v1/databases/" <> unpack databaseId <> "/query"
      result <- notionPost manager apiKey url NotionDatabaseQuery
      case result of
        Left _err -> pure []
        Right resp -> pure $ map (pageToTask notionCfg) (nqrResults resp)

  , opsUpdateTask = \task -> liftIO $ do
      let url = "https://api.notion.com/v1/pages/" <> unpack (unTaskId (taskId task))
          props = taskToNotionProperties notionCfg task
      _ <- notionPatch manager apiKey url (object ["properties" .= props]) :: IO (Either String Value)
      pure ()

  , opsArchiveTask = \tid -> liftIO $ do
      let url = "https://api.notion.com/v1/pages/" <> unpack (unTaskId tid)
      _ <- notionPatch manager apiKey url (object ["archived" .= True]) :: IO (Either String Value)
      pure ()
  }

taskToNotionProperties :: NotionConfig -> Task -> Value
taskToNotionProperties cfg task = object $ concat
  [ [ fromText (notionPropName cfg) .= object
        [ "title" .= [ object [ "text" .= object [ "content" .= taskTitle task ] ] ]
        ]
    ]
  , [ fromText (notionPropPriority cfg) .= object
        [ "select" .= object [ "name" .= priorityToText (taskPriority task) ]
        ]
    | not (T.null (notionPropPriority cfg))
    ]
  , [ fromText (notionPropStatus cfg) .= object
        [ "status" .= object [ "name" .= statusToText cfg (taskStatus task) ]
        ]
    | not (T.null (notionPropStatus cfg))
    ]
  , case (T.null (notionPropDueDate cfg), taskDueDate task) of
      (False, Just d) ->
        [ fromText (notionPropDueDate cfg) .= object
            [ "date" .= object [ "start" .= dayToText d ]
            ]
        ]
      _ -> []
  , [ fromText (notionPropAssignee cfg) .= object
        [ "people" .=
            [ object [ "object" .= ("user" :: Text), "id" .= notionAssigneeUserId cfg ] ]
        ]
    | not (T.null (notionPropAssignee cfg))
    , not (T.null (notionAssigneeUserId cfg))
    ]
  ]

dayToText :: Day -> Text
dayToText = T.pack . show

taskToNotionBody :: Task -> [Value]
taskToNotionBody task =
  [ object
      [ "object" .= ("block" :: Text)
      , "type" .= ("paragraph" :: Text)
      , "paragraph" .= object
          [ "rich_text" .=
              [ object
                  [ "type" .= ("text" :: Text)
                  , "text" .= object [ "content" .= taskDescription task ]
                  ]
              ]
          ]
      ]
  ]

priorityToText :: Priority -> Text
priorityToText Critical = "Critical"
priorityToText High     = "High"
priorityToText Medium   = "Medium"
priorityToText Low      = "Low"

statusToText :: NotionConfig -> TaskStatus -> Text
statusToText cfg Open       = notionStatusOpen cfg
statusToText cfg InProgress = notionStatusInProgress cfg
statusToText cfg Done       = notionStatusDone cfg
statusToText cfg Archived   = notionStatusDone cfg

pageToTask :: NotionConfig -> NotionPage -> Task
pageToTask cfg page =
  let props = npgProperties page
      epoch = UTCTime (fromGregorian 2000 1 1) 0
  in Task
  { taskId = TaskId (npgId page)
  , taskTitle = extractTitle (notionPropName cfg) props
  , taskDescription = ""
  , taskPriority = textToPriority (extractSelect (notionPropPriority cfg) props)
  , taskStatus = textToStatus cfg (extractStatus (notionPropStatus cfg) props)
  , taskDueDate = extractDate (notionPropDueDate cfg) props
  , taskEstimate = extractRichText (notionPropEstimate cfg) props
  , taskSource = Nothing
  , taskCreatedAt = epoch
  , taskUpdatedAt = epoch
  }

-- | Extract title from a Notion title property
extractTitle :: Text -> KM.KeyMap Value -> Text
extractTitle propName props
  | T.null propName = ""
  | otherwise = case KM.lookup (fromText propName) props of
      Just (Object obj) -> case KM.lookup "title" obj of
        Just (Array arr) -> case toList arr of
          (Object rt : _) -> case KM.lookup "plain_text" rt of
            Just (String t) -> t
            _ -> ""
          _ -> ""
        _ -> ""
      _ -> ""
  where
    toList v = foldr (:) [] v

-- | Extract select property value
extractSelect :: Text -> KM.KeyMap Value -> Text
extractSelect propName props
  | T.null propName = ""
  | otherwise = case KM.lookup (fromText propName) props of
      Just (Object obj) -> case KM.lookup "select" obj of
        Just (Object sel) -> case KM.lookup "name" sel of
          Just (String t) -> t
          _ -> ""
        Just Null -> ""
        _ -> ""
      _ -> ""

-- | Extract status property value
extractStatus :: Text -> KM.KeyMap Value -> Text
extractStatus propName props
  | T.null propName = ""
  | otherwise = case KM.lookup (fromText propName) props of
      Just (Object obj) -> case KM.lookup "status" obj of
        Just (Object st) -> case KM.lookup "name" st of
          Just (String t) -> t
          _ -> ""
        _ -> ""
      _ -> ""

-- | Extract date property value
extractDate :: Text -> KM.KeyMap Value -> Maybe Day
extractDate propName props
  | T.null propName = Nothing
  | otherwise = case KM.lookup (fromText propName) props of
      Just (Object obj) -> case KM.lookup "date" obj of
        Just (Object dt) -> case KM.lookup "start" dt of
          Just (String t) -> parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack t)
          _ -> Nothing
        Just Null -> Nothing
        _ -> Nothing
      _ -> Nothing

-- | Extract rich_text property value
extractRichText :: Text -> KM.KeyMap Value -> Maybe Text
extractRichText propName props
  | T.null propName = Nothing
  | otherwise = case KM.lookup (fromText propName) props of
      Just (Object obj) -> case KM.lookup "rich_text" obj of
        Just (Array arr) -> case toList arr of
          (Object rt : _) -> case KM.lookup "plain_text" rt of
            Just (String t) | not (T.null t) -> Just t
            _ -> Nothing
          _ -> Nothing
        _ -> Nothing
      _ -> Nothing
  where
    toList v = foldr (:) [] v

textToPriority :: Text -> Priority
textToPriority t = case T.toLower t of
  "critical" -> Critical
  "high"     -> High
  "medium"   -> Medium
  "low"      -> Low
  _          -> Medium

textToStatus :: NotionConfig -> Text -> TaskStatus
textToStatus cfg t
  | t == notionStatusDone cfg       = Done
  | t == notionStatusInProgress cfg = InProgress
  | t == notionStatusOpen cfg       = Open
  | otherwise                       = Open
