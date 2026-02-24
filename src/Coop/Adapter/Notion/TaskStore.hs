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
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Data.Time (Day)
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
        Right resp -> pure $ map pageToTask (nqrResults resp)

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

pageToTask :: NotionPage -> Task
pageToTask _page = Task
  { taskId = TaskId (npgId _page)
  , taskTitle = ""       -- Would parse from properties
  , taskDescription = "" -- Would parse from properties
  , taskPriority = Medium
  , taskStatus = Open
  , taskDueDate = Nothing
  , taskSource = error "pageToTask: source not available from Notion"
  , taskCreatedAt = error "pageToTask: createdAt not parsed"
  , taskUpdatedAt = error "pageToTask: updatedAt not parsed"
  }
