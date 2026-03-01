module Coop.Agent.CatchUp
  ( runCatchUp
  ) where

import Coop.Adapter.Slack.History (fetchChannelHistory)
import Coop.Agent.Core (processEvent)
import Coop.App.Env (Env (..))
import Coop.App.Log (logInfo)
import Coop.App.Monad (AppM, runAppM)
import Coop.Config (Config (..), RunMode (..), SlackConfig (..))
import Data.Aeson (Object, Value (..))
import Data.Aeson.Key (fromString)
import qualified Data.Aeson.KeyMap as KM
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory (createDirectoryIfMissing, doesFileExist, getXdgDirectory, XdgDirectory (..))
import System.FilePath ((</>))

tsFilePath :: IO FilePath
tsFilePath = do
  dir <- getXdgDirectory XdgConfig "coop"
  createDirectoryIfMissing True dir
  pure (dir </> "last-processed-ts")

readLastTs :: IO (Maybe Text)
readLastTs = do
  path <- tsFilePath
  exists <- doesFileExist path
  if exists
    then do
      content <- TIO.readFile path
      let ts = T.strip content
      if T.null ts then pure Nothing else pure (Just ts)
    else pure Nothing

writeLastTs :: Text -> IO ()
writeLastTs ts = do
  path <- tsFilePath
  TIO.writeFile path ts

currentSlackTs :: IO Text
currentSlackTs = do
  now <- getPOSIXTime
  pure $ T.pack (show (realToFrac now :: Double))

parseChannels :: Text -> [Text]
parseChannels = filter (not . T.null) . map T.strip . T.splitOn ","

lookupText :: Text -> Object -> Maybe Text
lookupText key obj = case KM.lookup (fromString (T.unpack key)) obj of
  Just (String t) -> Just t
  _               -> Nothing

-- | Run catch-up at startup: fetch messages that arrived while offline.
runCatchUp :: Env AppM -> IO ()
runCatchUp env = do
  let config   = envConfig env
      mode     = cfgMode config
      slackCfg = cfgSlack config
      channels = parseChannels (slackCatchupChannels slackCfg)
      botToken = slackBotToken slackCfg
      manager  = envHttpManager env

  -- Dryrun mode: skip
  case mode of
    Dryrun -> do
      runAppM env $ logInfo "CatchUp: Dryrun mode, skipping catch-up"
      pure ()
    Live -> do
      when (null channels) $ do
        runAppM env $ logInfo "CatchUp: No catch-up channels configured, skipping"

      unless (null channels) $ do
        mLastTs <- readLastTs

        case mLastTs of
          Nothing -> do
            -- First run: write current ts and skip
            ts <- currentSlackTs
            writeLastTs ts
            runAppM env $ logInfo "CatchUp: First run, recording current timestamp and skipping"

          Just lastTs -> do
            runAppM env $ logInfo $ "CatchUp: Fetching messages since " <> lastTs
                <> " from " <> T.pack (show (length channels)) <> " channel(s)"

            totalCount <- fmap sum $ forM channels $ \chanId -> do
              msgs <- fetchChannelHistory manager botToken chanId (Just lastTs)
              -- Add channel field (not included in API response) and sort oldest-first
              let withChannel = map (KM.insert (fromString "channel") (String chanId)) msgs
                  sorted = sortOn (\o -> lookupText "ts" o) withChannel
              -- Process each message
              forM_ sorted $ \msgObj -> do
                runAppM env $ processEvent msgObj
                -- Update last-processed ts after each successful message
                case lookupText "ts" msgObj of
                  Just ts -> writeLastTs ts
                  Nothing -> pure ()
              pure (length sorted)

            runAppM env $ logInfo $ "CatchUp: Processed " <> T.pack (show totalCount) <> " message(s)"
  where
    when cond action = if cond then action else pure ()
    unless cond action = if cond then pure () else action
    forM [] _ = pure []
    forM (x:xs) f = do
      r <- f x
      rs <- forM xs f
      pure (r : rs)
    forM_ [] _ = pure ()
    forM_ (x:xs) f = f x >> forM_ xs f
