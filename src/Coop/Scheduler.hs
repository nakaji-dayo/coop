module Coop.Scheduler
  ( runScheduler
  ) where

import Coop.Agent.Core (dailyBriefing)
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
  let cronExpr = schedulerBriefingCron (cfgScheduler (envConfig env))
  if cronExpr == "-"
    then TIO.putStrLn "Scheduler disabled (cron = \"-\")"
    else do
      TIO.putStrLn $ "Scheduler enabled, cron: " <> cronExpr
      tids <- execSchedule $ do
        addJob (runBriefing env) cronExpr
      -- execSchedule returns immediately with thread IDs;
      -- block forever so race_ keeps this alive
      TIO.putStrLn $ "Scheduler started (" <> T.pack (show (length tids)) <> " threads)"
      forever (threadDelay maxBound)

runBriefing :: Env AppM -> IO ()
runBriefing env = do
  TIO.putStrLn "Scheduler: running daily briefing"
  result <- try (runAppM env dailyBriefing) :: IO (Either SomeException ())
  case result of
    Left err -> TIO.putStrLn $ "Scheduler: briefing failed: " <> T.pack (show err)
    Right () -> TIO.putStrLn "Scheduler: briefing completed"

forever :: IO () -> IO ()
forever act = act >> forever act
