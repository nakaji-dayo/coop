module Coop.Adapter.GoogleCalendar.Client
  ( fetchEvents
  ) where

import Coop.Adapter.GoogleCalendar.Types (GoogleEventsResponse (..), GoogleEvent (..),
                                           GoogleEventDateTime (..), GoogleAttendee (..))
import Coop.Domain.Calendar (CalendarEvent (..), ResponseStatus (..))
import Data.Aeson (eitherDecode)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (Day, UTCTime (..), TimeZone, LocalTime (..), localTimeToUTC, midnight, secondsToDiffTime, formatTime, defaultTimeLocale)
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Client (Manager, parseRequest, httpLbs, requestHeaders,
                            responseBody, responseStatus)
import Network.HTTP.Types.Status (statusCode)

-- | Fetch calendar events for a given day (local timezone aware)
fetchEvents :: Manager -> Text -> Text -> Day -> TimeZone -> IO (Either String [CalendarEvent])
fetchEvents manager calendarId accessToken day tz = do
  let dayStart = localTimeToUTC tz (LocalTime day midnight)
      nextDay  = succ day
      dayEnd   = localTimeToUTC tz (LocalTime nextDay midnight)
      timeMin  = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" dayStart
      timeMax  = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" dayEnd
      url = T.concat
        [ "https://www.googleapis.com/calendar/v3/calendars/"
        , calendarId
        , "/events"
        , "?timeMin=", T.pack timeMin
        , "&timeMax=", T.pack timeMax
        , "&singleEvents=true"
        , "&orderBy=startTime"
        ]
  initReq <- parseRequest (T.unpack url)
  let req = initReq
        { requestHeaders =
            [ ("Authorization", "Bearer " <> TE.encodeUtf8 accessToken)
            , ("Accept", "application/json")
            ]
        }
  resp <- httpLbs req manager
  let status = statusCode (responseStatus resp)
  if status == 200
    then case eitherDecode (responseBody resp) :: Either String GoogleEventsResponse of
      Left err -> pure $ Left $ "Failed to parse events: " <> err
      Right eventsResp -> pure $ Right $ map toCalendarEvent (gerItems eventsResp)
    else pure $ Left $ "Google Calendar API error (HTTP " <> show status <> "): "
                     <> show (LBS.toStrict (responseBody resp))

-- | Convert a Google API event to our domain type
toCalendarEvent :: GoogleEvent -> CalendarEvent
toCalendarEvent ge = CalendarEvent
  { calTitle          = fromMaybe "(No title)" (geSummary ge)
  , calStart          = resolveDateTime (geStart ge)
  , calEnd            = resolveDateTime (geEnd ge)
  , calResponseStatus = selfResponseStatus (geAttendees ge)
  }

-- | Resolve a GoogleEventDateTime to UTCTime (timed events or all-day fallback)
resolveDateTime :: GoogleEventDateTime -> UTCTime
resolveDateTime gedt = case gedtDateTime gedt of
  Just utc -> utc
  Nothing  -> case gedtDate gedt of
    Just _ -> UTCTime (toEnum 0) (secondsToDiffTime 0)  -- all-day event fallback
    Nothing -> UTCTime (toEnum 0) (secondsToDiffTime 0)

-- | Find the self attendee's response status, defaulting to Accepted
selfResponseStatus :: [GoogleAttendee] -> ResponseStatus
selfResponseStatus attendees =
  case filter (\a -> gaSelf a == Just True) attendees of
    (a:_) -> parseResponseStatus (fromMaybe "accepted" (gaResponseStatus a))
    []    -> Accepted  -- No attendees list = organizer/own event

parseResponseStatus :: Text -> ResponseStatus
parseResponseStatus status = case T.toLower status of
  "accepted"    -> Accepted
  "declined"    -> Declined
  "tentative"   -> Tentative
  "needsAction" -> NeedsAction
  _             -> Accepted
