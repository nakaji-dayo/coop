module Coop.Domain.Calendar
  ( CalendarEvent (..)
  , ResponseStatus (..)
  ) where

import Data.Text (Text)
import Data.Time (UTCTime)

data ResponseStatus = Accepted | Declined | Tentative | NeedsAction
  deriving stock (Eq, Show)

data CalendarEvent = CalendarEvent
  { calTitle          :: Text
  , calStart          :: UTCTime
  , calEnd            :: UTCTime
  , calResponseStatus :: ResponseStatus
  } deriving stock (Eq, Show)
