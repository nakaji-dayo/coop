module Coop.Domain.Calendar
  ( CalendarEvent (..)
  , ResponseStatus (..)
  , Visibility (..)
  ) where

import Data.Text (Text)
import Data.Time (UTCTime)

data ResponseStatus = Accepted | Declined | Tentative | NeedsAction
  deriving stock (Eq, Show)

data Visibility = Public | Private | Confidential | DefaultVisibility
  deriving stock (Eq, Show)

data CalendarEvent = CalendarEvent
  { calTitle          :: Text
  , calStart          :: UTCTime
  , calEnd            :: UTCTime
  , calResponseStatus :: ResponseStatus
  , calVisibility     :: Visibility
  } deriving stock (Eq, Show)
