module Coop.Domain.MentionSpec (spec) where

import Test.Hspec

import Coop.Domain.Mention

spec :: Spec
spec = do
  describe "isBotMentioned" $ do
    it "detects bot mention in text" $ do
      isBotMentioned "U123BOT" "<@U123BOT> hello" `shouldBe` True

    it "detects bot mention in middle of text" $ do
      isBotMentioned "U123BOT" "hey <@U123BOT> do this" `shouldBe` True

    it "returns False when bot is not mentioned" $ do
      isBotMentioned "U123BOT" "hello world" `shouldBe` False

    it "returns False for partial mention" $ do
      isBotMentioned "U123BOT" "<@U123BO" `shouldBe` False

    it "does not match different user mention" $ do
      isBotMentioned "U123BOT" "<@U999OTHER> hello" `shouldBe` False

  describe "stripBotMention" $ do
    it "removes mention and trims whitespace" $ do
      stripBotMention "U123BOT" "<@U123BOT> hello world" `shouldBe` "hello world"

    it "handles mention in middle of text" $ do
      stripBotMention "U123BOT" "hey <@U123BOT> do this" `shouldBe` "hey  do this"

    it "returns empty for mention-only text" $ do
      stripBotMention "U123BOT" "<@U123BOT>" `shouldBe` ""

  describe "parseMention" $ do
    it "parses a valid mention" $ do
      let result = parseMention "U123BOT" "C001" "1234.5678" "U999" (Just "alice") "<@U123BOT> please fix the bug"
      case result of
        Nothing -> expectationFailure "Expected Just"
        Just pm -> do
          pmChannel pm `shouldBe` "C001"
          pmTimestamp pm `shouldBe` "1234.5678"
          pmUserId pm `shouldBe` "U999"
          pmUserName pm `shouldBe` Just "alice"
          pmStrippedText pm `shouldBe` "please fix the bug"
          pmMentionType pm `shouldBe` BotCommand

    it "returns Nothing when bot is not mentioned" $ do
      let result = parseMention "U123BOT" "C001" "1234.5678" "U999" Nothing "hello world"
      result `shouldBe` Nothing

    it "classifies mention-only as DirectMention" $ do
      let result = parseMention "U123BOT" "C001" "1234.5678" "U999" Nothing "<@U123BOT>"
      case result of
        Nothing -> expectationFailure "Expected Just"
        Just pm -> pmMentionType pm `shouldBe` DirectMention
