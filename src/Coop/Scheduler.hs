module Coop.Scheduler
  ( runScheduler
  ) where

import Coop.Agent.Core (dailyBriefing, weeklyBriefing)
import Coop.App.Env (Env (..))
import Coop.App.Monad (AppM, runAppM)
import Coop.Config (Config (..), SchedulerConfig (..))
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Cron (execSchedule, addJob)

runScheduler :: Env AppM -> IO ()
runScheduler env = do
  let schedCfg = cfgScheduler (envConfig env)
      dailyCron = schedulerBriefingCron schedCfg
      weeklyCron = schedulerWeeklyBriefingCron schedCfg
  if dailyCron == "-" && weeklyCron == "-"
    then TIO.putStrLn "Scheduler disabled (all crons = \"-\")"
    else do
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
    Right () -> TIO.putStrLn "Scheduler: daily briefing completed"

runWeeklyBriefing :: Env AppM -> IO ()
runWeeklyBriefing env = do
  TIO.putStrLn "Scheduler: running weekly briefing"
  result <- try (runAppM env weeklyBriefing) :: IO (Either SomeException ())
  case result of
    Left err -> TIO.putStrLn $ "Scheduler: weekly briefing failed: " <> T.pack (show err)
    Right () -> TIO.putStrLn "Scheduler: weekly briefing completed"

forever :: IO () -> IO ()
forever act = act >> forever act
