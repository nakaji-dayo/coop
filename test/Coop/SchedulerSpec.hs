module Coop.SchedulerSpec (spec) where

import Test.Hspec
import Coop.Scheduler (shouldRunCatchUp)
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import qualified Data.Text as T
import System.Cron (parseCronSchedule, CronSchedule)

parseTime :: String -> UTCTime
parseTime s = case iso8601ParseM s of
  Just t  -> t
  Nothing -> error $ "Invalid time: " <> s

parseCron :: String -> CronSchedule
parseCron s = case parseCronSchedule (T.pack s) of
  Right cs -> cs
  Left err -> error $ "Invalid cron: " <> err

spec :: Spec
spec = do
  describe "shouldRunCatchUp" $ do
    it "returns True when a scheduled run was missed" $ do
      let cs      = parseCron "0 9 * * *"
          lastRun = parseTime "2024-01-01T09:00:00Z"
          now     = parseTime "2024-01-02T10:00:00Z"
      shouldRunCatchUp cs lastRun now `shouldBe` True

    it "returns False when next schedule is still in the future" $ do
      let cs      = parseCron "0 9 * * *"
          lastRun = parseTime "2024-01-01T09:00:00Z"
          now     = parseTime "2024-01-02T08:00:00Z"
      shouldRunCatchUp cs lastRun now `shouldBe` False

    it "returns True when multiple runs were missed" $ do
      let cs      = parseCron "0 9 * * *"
          lastRun = parseTime "2024-01-01T09:00:00Z"
          now     = parseTime "2024-01-05T12:00:00Z"
      shouldRunCatchUp cs lastRun now `shouldBe` True

    it "returns True for weekly schedule when missed" $ do
      let cs      = parseCron "0 9 * * 1"
          lastRun = parseTime "2024-01-01T09:00:00Z"
          now     = parseTime "2024-01-08T10:00:00Z"
      shouldRunCatchUp cs lastRun now `shouldBe` True

    it "returns False for weekly schedule when not yet due" $ do
      let cs      = parseCron "0 9 * * 1"
          lastRun = parseTime "2024-01-01T09:00:00Z"
          now     = parseTime "2024-01-03T10:00:00Z"
      shouldRunCatchUp cs lastRun now `shouldBe` False
