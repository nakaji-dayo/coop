module Coop.Adapter.GoogleCalendar.Types
  ( GoogleEventsResponse (..)
  , GoogleEvent (..)
  , GoogleEventDateTime (..)
  , GoogleAttendee (..)
  , GoogleTokenResponse (..)
  , GoogleToken (..)
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), withObject, (.:), (.:?), (.=), object)
import Data.Text (Text)
import Data.Time (UTCTime)

-- | Google Calendar Events.list response
data GoogleEventsResponse = GoogleEventsResponse
  { gerItems :: [GoogleEvent]
  } deriving stock (Eq, Show)

instance FromJSON GoogleEventsResponse where
  parseJSON = withObject "GoogleEventsResponse" $ \v ->
    GoogleEventsResponse <$> (v .:? "items" >>= pure . maybe [] id)

-- | A single calendar event from Google API
data GoogleEvent = GoogleEvent
  { geId         :: Text
  , geSummary    :: Maybe Text
  , geStart      :: GoogleEventDateTime
  , geEnd        :: GoogleEventDateTime
  , geAttendees  :: [GoogleAttendee]
  , geVisibility :: Maybe Text
  } deriving stock (Eq, Show)

instance FromJSON GoogleEvent where
  parseJSON = withObject "GoogleEvent" $ \v ->
    GoogleEvent
      <$> v .: "id"
      <*> v .:? "summary"
      <*> v .: "start"
      <*> v .: "end"
      <*> (v .:? "attendees" >>= pure . maybe [] id)
      <*> v .:? "visibility"

-- | Event start/end time — supports both timed and all-day events
data GoogleEventDateTime = GoogleEventDateTime
  { gedtDateTime :: Maybe UTCTime
  , gedtDate     :: Maybe Text   -- ^ "YYYY-MM-DD" for all-day events
  } deriving stock (Eq, Show)

instance FromJSON GoogleEventDateTime where
  parseJSON = withObject "GoogleEventDateTime" $ \v ->
    GoogleEventDateTime
      <$> v .:? "dateTime"
      <*> v .:? "date"

-- | Attendee info
data GoogleAttendee = GoogleAttendee
  { gaEmail          :: Text
  , gaResponseStatus :: Maybe Text
  , gaSelf           :: Maybe Bool
  } deriving stock (Eq, Show)

instance FromJSON GoogleAttendee where
  parseJSON = withObject "GoogleAttendee" $ \v ->
    GoogleAttendee
      <$> v .: "email"
      <*> v .:? "responseStatus"
      <*> v .:? "self"

-- | OAuth token response from Google
data GoogleTokenResponse = GoogleTokenResponse
  { gtrAccessToken  :: Text
  , gtrExpiresIn    :: Int
  , gtrRefreshToken :: Maybe Text
  , gtrTokenType    :: Text
  } deriving stock (Eq, Show)

instance FromJSON GoogleTokenResponse where
  parseJSON = withObject "GoogleTokenResponse" $ \v ->
    GoogleTokenResponse
      <$> v .: "access_token"
      <*> v .: "expires_in"
      <*> v .:? "refresh_token"
      <*> v .: "token_type"

-- | Locally stored token with expiry timestamp
data GoogleToken = GoogleToken
  { gtAccessToken  :: Text
  , gtRefreshToken :: Text
  , gtExpiresAt    :: UTCTime
  } deriving stock (Eq, Show)

instance FromJSON GoogleToken where
  parseJSON = withObject "GoogleToken" $ \v ->
    GoogleToken
      <$> v .: "access_token"
      <*> v .: "refresh_token"
      <*> v .: "expires_at"

instance ToJSON GoogleToken where
  toJSON t = object
    [ "access_token"  .= gtAccessToken t
    , "refresh_token" .= gtRefreshToken t
    , "expires_at"    .= gtExpiresAt t
    ]
