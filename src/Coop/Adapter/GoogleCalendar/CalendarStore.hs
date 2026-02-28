module Coop.Adapter.GoogleCalendar.CalendarStore
  ( mkLiveCalendarStoreOps
  , mkNoopCalendarStoreOps
  ) where

import Coop.Adapter.GoogleCalendar.Auth (loadAndRefreshToken)
import Coop.Adapter.GoogleCalendar.Client (fetchEvents)
import Coop.Adapter.GoogleCalendar.Types (GoogleToken (..))
import Coop.App.Env (CalendarStoreOps (..))
import Coop.Config (GoogleCalendarConfig (..))
import Control.Exception (try, SomeException)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time.LocalTime (getCurrentTimeZone)
import Network.HTTP.Client (Manager)

-- | Live CalendarStore that calls Google Calendar API
mkLiveCalendarStoreOps :: (MonadIO m) => GoogleCalendarConfig -> Manager -> CalendarStoreOps m
mkLiveCalendarStoreOps cfg manager = CalendarStoreOps
  { opsGetEvents = \day -> liftIO $ do
      result <- try $ do
        tz <- getCurrentTimeZone
        token <- loadAndRefreshToken cfg manager
        fetchEvents manager (googleCalendarId cfg) (gtAccessToken token) day tz
      case result of
        Left (err :: SomeException) -> do
          putStrLn $ "[CalendarStore] Error: " <> show err
          pure []
        Right (Left err) -> do
          putStrLn $ "[CalendarStore] API error: " <> err
          pure []
        Right (Right events) -> pure events
  }

-- | Noop CalendarStore that always returns empty (for when calendar is not configured)
mkNoopCalendarStoreOps :: (Applicative m) => CalendarStoreOps m
mkNoopCalendarStoreOps = CalendarStoreOps
  { opsGetEvents = \_ -> pure []
  }
