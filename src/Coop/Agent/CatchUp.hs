module Coop.Agent.CatchUp
  ( runCatchUp
  , runCatchUpWith
  , CatchUpDeps (..)
  , parseChannels
  ) where

import Control.Exception (SomeException, try)
import Coop.Adapter.Slack.History (fetchChannelHistory)
import Coop.Agent.Core (processEvent)
import Coop.App.Env (Env (..))
import Coop.App.Log (logInfo, logWarn, logError)
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

-- | Injectable dependencies for catch-up logic.
data CatchUpDeps = CatchUpDeps
  { depFetchHistory :: Text -> Maybe Text -> IO (Either Text [Object])
  , depReadLastTs   :: IO (Maybe Text)
  , depWriteLastTs  :: Text -> IO ()
  , depCurrentTs    :: IO Text
  }

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

-- | Production entry point. Builds real deps and delegates to 'runCatchUpWith'.
runCatchUp :: Env AppM -> IO ()
runCatchUp env = do
  let slackCfg = cfgSlack (envConfig env)
      botToken = slackBotToken slackCfg
      manager  = envHttpManager env
      deps = CatchUpDeps
        { depFetchHistory = \chanId mOldest -> fetchChannelHistory manager botToken chanId mOldest
        , depReadLastTs   = readLastTs
        , depWriteLastTs  = writeLastTs
        , depCurrentTs    = currentSlackTs
        }
  runCatchUpWith deps env

-- | Testable catch-up logic with injected dependencies.
--   Never throws — all errors are logged and catch-up is skipped gracefully.
runCatchUpWith :: CatchUpDeps -> Env AppM -> IO ()
runCatchUpWith deps env = do
  result <- try (runCatchUpInner deps env)
  case result of
    Left (e :: SomeException) ->
      runAppM env $ logError $ "CatchUp: Unexpected error, skipping catch-up: " <> T.pack (show e)
    Right () -> pure ()

runCatchUpInner :: CatchUpDeps -> Env AppM -> IO ()
runCatchUpInner deps env = do
  let config   = envConfig env
      mode     = cfgMode config
      channels = parseChannels (slackCatchupChannels (cfgSlack config))

  case mode of
    Dryrun -> do
      runAppM env $ logInfo "CatchUp: Dryrun mode, skipping catch-up"
      pure ()
    Live -> do
      when (null channels) $ do
        runAppM env $ logInfo "CatchUp: No catch-up channels configured, skipping"

      unless (null channels) $ do
        mLastTs <- depReadLastTs deps

        case mLastTs of
          Nothing -> do
            ts <- depCurrentTs deps
            depWriteLastTs deps ts
            runAppM env $ logInfo "CatchUp: First run, recording current timestamp and skipping"

          Just lastTs -> do
            runAppM env $ logInfo $ "CatchUp: Fetching messages since " <> lastTs
                <> " from " <> T.pack (show (length channels)) <> " channel(s)"

            totalCount <- fmap sum $ forM channels $ \chanId -> do
              historyResult <- depFetchHistory deps chanId (Just lastTs)
              case historyResult of
                Left err -> do
                  runAppM env $ logWarn $ "CatchUp: Failed to fetch history for channel " <> chanId <> ": " <> err
                  pure 0
                Right msgs -> do
                  let withChannel = map (KM.insert (fromString "channel") (String chanId)) msgs
                      sorted = sortOn (\o -> lookupText "ts" o) withChannel
                  processCount <- fmap sum $ forM sorted $ \msgObj -> do
                    result <- try (runAppM env $ processEvent msgObj)
                    case result of
                      Left (e :: SomeException) -> do
                        runAppM env $ logWarn $ "CatchUp: Failed to process message ts="
                            <> maybe "?" id (lookupText "ts" msgObj) <> ": " <> T.pack (show e)
                        pure 0
                      Right () -> do
                        case lookupText "ts" msgObj of
                          Just ts -> depWriteLastTs deps ts
                          Nothing -> pure ()
                        pure (1 :: Int)
                  pure processCount

            runAppM env $ logInfo $ "CatchUp: Processed " <> T.pack (show totalCount) <> " message(s)"
  where
    when cond action = if cond then action else pure ()
    unless cond action = if cond then pure () else action
    forM [] _ = pure []
    forM (x:xs) f = do
      r <- f x
      rs <- forM xs f
      pure (r : rs)
