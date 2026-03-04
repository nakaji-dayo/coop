module Coop.Scheduler
  ( runScheduler
  , shouldRunCatchUp
  ) where

import Coop.Agent.Core (dailyBriefing, weeklyBriefing)
import Coop.App.Env (Env (..))
import Coop.App.Monad (AppM, runAppM)
import Coop.Config (Config (..), SchedulerConfig (..))
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show, iso8601ParseM)
import System.Cron (execSchedule, addJob, nextMatch, CronSchedule, parseCronSchedule)
import System.Directory (createDirectoryIfMissing, doesFileExist, getXdgDirectory, XdgDirectory (..))
import System.FilePath ((</>))

-- | Determine whether a catch-up run is needed.
-- Returns True if a scheduled execution was missed between lastRunTime and now.
shouldRunCatchUp :: CronSchedule -> UTCTime -> UTCTime -> Bool
shouldRunCatchUp cs lastRunTime now =
  case nextMatch cs lastRunTime of
    Nothing -> False
    Just nextTime -> nextTime <= now

-- Timestamp persistence

schedulerFilePath :: String -> IO FilePath
schedulerFilePath name = do
  dir <- getXdgDirectory XdgConfig "coop"
  createDirectoryIfMissing True dir
  pure (dir </> ("scheduler-" <> name <> "-last-run"))

readLastRunTime :: String -> IO (Maybe UTCTime)
readLastRunTime name = do
  path <- schedulerFilePath name
  exists <- doesFileExist path
  if exists
    then do
      content <- readFile path
      let trimmed = filter (/= '\n') content
      pure (iso8601ParseM trimmed)
    else pure Nothing

writeLastRunTime :: String -> UTCTime -> IO ()
writeLastRunTime name t = do
  path <- schedulerFilePath name
  writeFile path (iso8601Show t)

-- Briefing catch-up at startup

runBriefingCatchUp :: Env AppM -> IO ()
runBriefingCatchUp env = do
  let schedCfg = cfgScheduler (envConfig env)
  catchUpBriefing "daily" (schedulerBriefingCron schedCfg) (runDailyBriefing env)
  catchUpBriefing "weekly" (schedulerWeeklyBriefingCron schedCfg) (runWeeklyBriefing env)

catchUpBriefing :: String -> Text -> IO () -> IO ()
catchUpBriefing name cronExpr action
  | cronExpr == "-" = pure ()
  | otherwise = do
      case parseCronSchedule cronExpr of
        Left err -> TIO.putStrLn $ "CatchUp: Failed to parse cron for " <> T.pack name <> ": " <> T.pack err
        Right cs -> do
          mLastRun <- readLastRunTime name
          now <- getCurrentTime
          case mLastRun of
            Nothing -> do
              writeLastRunTime name now
              TIO.putStrLn $ "CatchUp: First run for " <> T.pack name <> ", recording timestamp"
            Just lastRun ->
              if shouldRunCatchUp cs lastRun now
                then do
                  TIO.putStrLn $ "CatchUp: Missed " <> T.pack name <> " briefing, running catch-up"
                  action
                else TIO.putStrLn $ "CatchUp: " <> T.pack name <> " briefing is up to date"

-- Main scheduler

runScheduler :: Env AppM -> IO ()
runScheduler env = do
  let schedCfg = cfgScheduler (envConfig env)
      dailyCron = schedulerBriefingCron schedCfg
      weeklyCron = schedulerWeeklyBriefingCron schedCfg
  if dailyCron == "-" && weeklyCron == "-"
    then TIO.putStrLn "Scheduler disabled (all crons = \"-\")"
    else do
      runBriefingCatchUp env
      if dailyCron /= "-"
        then TIO.putStrLn $ "Daily briefing cron: " <> dailyCron
        else TIO.putStrLn "Daily briefing disabled"
      if weeklyCron /= "-"
        then TIO.putStrLn $ "Weekly briefing cron: " <> weeklyCron
        else TIO.putStrLn "Weekly briefing disabled"
      tids <- execSchedule $ do
        if dailyCron /= "-"
          then addJob (runDailyBriefing env) dailyCron
          else pure ()
        if weeklyCron /= "-"
          then addJob (runWeeklyBriefing env) weeklyCron
          else pure ()
      TIO.putStrLn $ "Scheduler started (" <> T.pack (show (length tids)) <> " threads)"
      forever (threadDelay maxBound)

runDailyBriefing :: Env AppM -> IO ()
runDailyBriefing env = do
  TIO.putStrLn "Scheduler: running daily briefing"
  result <- try (runAppM env dailyBriefing) :: IO (Either SomeException ())
  case result of
    Left err -> TIO.putStrLn $ "Scheduler: daily briefing failed: " <> T.pack (show err)
    Right () -> do
      now <- getCurrentTime
      writeLastRunTime "daily" now
      TIO.putStrLn "Scheduler: daily briefing completed"

runWeeklyBriefing :: Env AppM -> IO ()
runWeeklyBriefing env = do
  TIO.putStrLn "Scheduler: running weekly briefing"
  result <- try (runAppM env weeklyBriefing) :: IO (Either SomeException ())
  case result of
    Left err -> TIO.putStrLn $ "Scheduler: weekly briefing failed: " <> T.pack (show err)
    Right () -> do
      now <- getCurrentTime
      writeLastRunTime "weekly" now
      TIO.putStrLn "Scheduler: weekly briefing completed"

forever :: IO () -> IO ()
forever act = act >> forever act
