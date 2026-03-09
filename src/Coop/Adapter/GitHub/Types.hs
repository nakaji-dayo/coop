module Coop.Adapter.GitHub.Types
  ( GitHubIssue (..)
  , GitHubLabel (..)
  , GitHubCreateIssueRequest (..)
  , GitHubUpdateIssueRequest (..)
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), withObject, (.:), (.=), object)
import Data.Text (Text)
import GHC.Generics (Generic)

data GitHubLabel = GitHubLabel
  { ghlName :: Text
  } deriving stock (Eq, Show, Generic)

instance FromJSON GitHubLabel where
  parseJSON = withObject "GitHubLabel" $ \v ->
    GitHubLabel <$> v .: "name"

data GitHubIssue = GitHubIssue
  { ghiNumber  :: Int
  , ghiTitle   :: Text
  , ghiBody    :: Maybe Text
  , ghiState   :: Text
  , ghiLabels  :: [GitHubLabel]
  , ghiHtmlUrl :: Text
  } deriving stock (Eq, Show, Generic)

instance FromJSON GitHubIssue where
  parseJSON = withObject "GitHubIssue" $ \v ->
    GitHubIssue
      <$> v .: "number"
      <*> v .: "title"
      <*> v .: "body"
      <*> v .: "state"
      <*> v .: "labels"
      <*> v .: "html_url"

data GitHubCreateIssueRequest = GitHubCreateIssueRequest
  { gcrTitle  :: Text
  , gcrBody   :: Text
  , gcrLabels :: [Text]
  } deriving stock (Eq, Show, Generic)

instance ToJSON GitHubCreateIssueRequest where
  toJSON r = object
    [ "title"  .= gcrTitle r
    , "body"   .= gcrBody r
    , "labels" .= gcrLabels r
    ]

data GitHubUpdateIssueRequest = GitHubUpdateIssueRequest
  { gurState :: Text
  } deriving stock (Eq, Show, Generic)

instance ToJSON GitHubUpdateIssueRequest where
  toJSON r = object
    [ "state" .= gurState r
    ]
