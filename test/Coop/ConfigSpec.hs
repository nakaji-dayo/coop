module Coop.ConfigSpec (spec) where

import Test.Hspec
import Coop.Config (normalizeNotionId)

spec :: Spec
spec = do
  describe "normalizeNotionId" $ do
    it "returns raw 32-hex ID unchanged" $
      normalizeNotionId "a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4"
        `shouldBe` "a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4"

    it "extracts ID from a page URL" $
      normalizeNotionId "https://www.notion.so/workspace/a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4"
        `shouldBe` "a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4"

    it "extracts ID from a database URL with query params" $
      normalizeNotionId "https://www.notion.so/workspace/a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4?v=xyz"
        `shouldBe` "a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4"

    it "extracts ID from a slug URL" $
      normalizeNotionId "https://www.notion.so/workspace/My-Page-Title-a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4"
        `shouldBe` "a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4"

    it "extracts ID from a hyphenated UUID-style URL" $
      normalizeNotionId "https://www.notion.so/workspace/a1b2c3d4-e5f6-a1b2-c3d4-e5f6a1b2c3d4"
        `shouldBe` "a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4"

    it "returns empty string unchanged" $
      normalizeNotionId "" `shouldBe` ""
