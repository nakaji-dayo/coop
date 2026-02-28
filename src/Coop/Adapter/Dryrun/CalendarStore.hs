module Coop.Adapter.Dryrun.CalendarStore
  ( mkDryrunCalendarStoreOps
  ) where

import Coop.App.Env (CalendarStoreOps (..))
import Coop.Domain.Calendar (CalendarEvent (..), ResponseStatus (..))
import Data.Time (Day, UTCTime (..), secondsToDiffTime)

mkDryrunCalendarStoreOps :: (Applicative m) => CalendarStoreOps m
mkDryrunCalendarStoreOps = CalendarStoreOps
  { opsGetEvents = \day -> pure (mockEvents day)
  }

mockEvents :: Day -> [CalendarEvent]
mockEvents day =
  [ CalendarEvent
      { calTitle = "Standup"
      , calStart = UTCTime day (secondsToDiffTime (10 * 3600))
      , calEnd   = UTCTime day (secondsToDiffTime (10 * 3600 + 900))
      , calResponseStatus = Accepted
      }
  , CalendarEvent
      { calTitle = "1on1 with Manager"
      , calStart = UTCTime day (secondsToDiffTime (14 * 3600))
      , calEnd   = UTCTime day (secondsToDiffTime (14 * 3600 + 1800))
      , calResponseStatus = Accepted
      }
  , CalendarEvent
      { calTitle = "Design Review"
      , calStart = UTCTime day (secondsToDiffTime (16 * 3600))
      , calEnd   = UTCTime day (secondsToDiffTime (17 * 3600))
      , calResponseStatus = Tentative
      }
  ]
