module Coop.Adapter.Notion.Types
  ( NotionPage (..)
  , NotionBlock (..)
  , NotionRichText (..)
  , NotionDatabaseQuery (..)
  , NotionQueryResponse (..)
  , NotionCreatePageRequest (..)
  ) where

import Data.Aeson
import Data.Aeson.Key (fromText)
import Data.Text (Text)
import GHC.Generics (Generic)

data NotionRichText = NotionRichText
  { nrtPlainText :: Text
  } deriving stock (Eq, Show, Generic)

instance FromJSON NotionRichText where
  parseJSON = withObject "NotionRichText" $ \v ->
    NotionRichText <$> v .: "plain_text"

data NotionBlock = NotionBlock
  { nbId    :: Text
  , nbType  :: Text
  , nbTexts :: [NotionRichText]
  } deriving stock (Eq, Show, Generic)

instance FromJSON NotionBlock where
  parseJSON = withObject "NotionBlock" $ \v -> do
    bid <- v .: "id"
    btype <- v .: "type"
    let btypeKey = fromText btype
    content <- v .:? btypeKey
    texts <- case content of
      Just obj -> obj .:? "rich_text" >>= pure . maybe [] id
      Nothing  -> pure []
    pure $ NotionBlock bid btype texts

data NotionPage = NotionPage
  { npgId         :: Text
  , npgProperties :: Object
  } deriving stock (Eq, Show, Generic)

instance FromJSON NotionPage where
  parseJSON = withObject "NotionPage" $ \v -> NotionPage
    <$> v .: "id"
    <*> v .: "properties"

data NotionDatabaseQuery = NotionDatabaseQuery
  deriving stock (Eq, Show, Generic)

instance ToJSON NotionDatabaseQuery where
  toJSON _ = object []

data NotionQueryResponse = NotionQueryResponse
  { nqrResults :: [NotionPage]
  } deriving stock (Eq, Show, Generic)

instance FromJSON NotionQueryResponse where
  parseJSON = withObject "NotionQueryResponse" $ \v ->
    NotionQueryResponse <$> v .: "results"

data NotionCreatePageRequest = NotionCreatePageRequest
  { ncprParentDatabaseId :: Text
  , ncprProperties       :: Value
  , ncprChildren         :: [Value]
  } deriving stock (Eq, Show, Generic)

instance ToJSON NotionCreatePageRequest where
  toJSON (NotionCreatePageRequest dbId props children) = object $
    [ "parent" .= object ["database_id" .= dbId]
    , "properties" .= props
    ] <> [ "children" .= children | not (null children) ]
