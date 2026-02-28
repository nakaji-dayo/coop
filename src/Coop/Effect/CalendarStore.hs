module Coop.Effect.CalendarStore
  ( CalendarStore (..)
  ) where

import Coop.Domain.Calendar (CalendarEvent)
import Data.Time (Day)

class (Monad m) => CalendarStore m where
  getEvents :: Day -> m [CalendarEvent]
