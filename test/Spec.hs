module Main (main) where

import Test.Hspec

import qualified Coop.Domain.TaskSpec
import qualified Coop.Domain.MentionSpec
import qualified Coop.Agent.CatchUpSpec

main :: IO ()
main = hspec $ do
  describe "Domain.Task" Coop.Domain.TaskSpec.spec
  describe "Domain.Mention" Coop.Domain.MentionSpec.spec
  describe "Agent.CatchUp" Coop.Agent.CatchUpSpec.spec
