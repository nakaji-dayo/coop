module Coop.Domain.TaskSpec (spec) where

import Test.Hspec

import Data.Aeson (decode, encode)
import Coop.Domain.Task

spec :: Spec
spec = do
  describe "Priority" $ do
    it "JSON round-trips" $ do
      decode (encode Critical) `shouldBe` Just Critical
      decode (encode High)     `shouldBe` Just High
      decode (encode Medium)   `shouldBe` Just Medium
      decode (encode Low)      `shouldBe` Just Low

  describe "TaskStatus" $ do
    it "JSON round-trips" $ do
      decode (encode Open)       `shouldBe` Just Open
      decode (encode InProgress) `shouldBe` Just InProgress
      decode (encode Done)       `shouldBe` Just Done
      decode (encode Archived)   `shouldBe` Just Archived

  describe "TaskId" $ do
    it "JSON round-trips" $ do
      let tid = TaskId "test-123"
      decode (encode tid) `shouldBe` Just tid
