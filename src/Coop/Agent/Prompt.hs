module Coop.Agent.Prompt
  ( buildMentionAnalysisPrompt
  , buildBotCommandPrompt
  , MentionAnalysis (..)
  , AnalysisResult (..)
  , parseMentionAnalysis
  ) where

import Coop.Agent.Context (AgentContext (..))
import Coop.Domain.LLM (CompletionRequest (..), Message (..), Role (..))
import Coop.Domain.Mention (ParsedMention (..))
import Coop.Domain.Task (Task (..), Priority (..))
import Data.Aeson (FromJSON (..), withObject, (.:), (.:?), eitherDecodeStrict)
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (Day)
import GHC.Generics (Generic)

data MentionAnalysis = MentionAnalysis
  { maPriority    :: Priority
  , maTitle       :: Text
  , maDescription :: Text
  , maReason      :: Text
  , maDueDate     :: Maybe Day
  } deriving stock (Eq, Show, Generic)

data AnalysisResult
  = CreateTask MentionAnalysis
  | SkipTask Text  -- ^ reason for skipping
  deriving stock (Eq, Show)

instance FromJSON AnalysisResult where
  parseJSON = withObject "AnalysisResult" $ \v -> do
    action <- v .: "action" :: Parser Text
    reason <- v .: "reason"
    case action of
      "skip" -> pure $ SkipTask reason
      _ -> do
        prio <- v .: "priority"
        title <- v .: "title"
        desc <- v .: "description"
        dueDate <- v .:? "due_date"
        priority <- case (prio :: Text) of
          "Critical" -> pure Critical
          "High"     -> pure High
          "Medium"   -> pure Medium
          "Low"      -> pure Low
          _          -> pure Medium
        pure $ CreateTask $ MentionAnalysis priority title desc reason dueDate

parseMentionAnalysis :: Text -> Either String AnalysisResult
parseMentionAnalysis txt =
  let cleaned = extractJson txt
  in eitherDecodeStrict (TE.encodeUtf8 cleaned)

extractJson :: Text -> Text
extractJson txt
  | "```json" `T.isInfixOf` txt =
      let afterStart = T.drop 1 $ snd $ T.breakOn "\n" $ snd $ T.breakOn "```json" txt
          beforeEnd = fst $ T.breakOn "```" afterStart
      in T.strip beforeEnd
  | "{" `T.isInfixOf` txt =
      let afterBrace = snd $ T.breakOn "{" txt
          -- Find the matching closing brace (simple heuristic)
          (json, _) = T.breakOnEnd "}" afterBrace
      in T.strip json
  | otherwise = txt

buildMentionAnalysisPrompt :: AgentContext -> Day -> ParsedMention -> CompletionRequest
buildMentionAnalysisPrompt ctx today mention = CompletionRequest
  { crSystem = Just systemPrompt
  , crMessages =
      [ Message User userContent
      ]
  }
  where
    systemPrompt = T.unlines
      [ acInstructions ctx
      , ""
      , "## Behavioral Guidelines"
      , acGuidelines ctx
      , ""
      , "## Existing Tasks"
      , formatTasks (acExistingTasks ctx)
      ]

    userContent = T.unlines
      [ "A new mention was received:"
      , ""
      , "Today: " <> T.pack (show today)
      , "Channel: " <> pmChannel mention
      , "User: " <> pmUserId mention <> maybe "" (" (" <>) (pmUserName mention) <> maybe "" (const ")") (pmUserName mention)
      , "Message: " <> pmStrippedText mention
      , ""
      , "Analyze this mention and respond with a JSON object."
      , ""
      , "If this should be turned into a task:"
      , "{ \"action\": \"create_task\", \"priority\": \"Critical|High|Medium|Low\", \"title\": \"...\", \"description\": \"...\", \"reason\": \"...\", \"due_date\": \"YYYY-MM-DD or null\" }"
      , ""
      , "Set due_date when the message implies a deadline (e.g. \"明日までに\" → tomorrow's date, \"今週中\" → end of this week). Set null if no deadline is mentioned."
      , ""
      , "If this should NOT be a task (e.g. casual conversation, greetings, etc.):"
      , "{ \"action\": \"skip\", \"reason\": \"...\" }"
      ]

buildBotCommandPrompt :: AgentContext -> ParsedMention -> CompletionRequest
buildBotCommandPrompt ctx mention = CompletionRequest
  { crSystem = Just systemPrompt
  , crMessages =
      [ Message User (pmStrippedText mention)
      ]
  }
  where
    systemPrompt = T.unlines
      [ acInstructions ctx
      , ""
      , "## Behavioral Guidelines"
      , acGuidelines ctx
      , ""
      , "## Current Tasks"
      , formatTasks (acExistingTasks ctx)
      , ""
      , "The user is talking to you directly. Respond helpfully and concisely."
      , "If they ask to re-evaluate a task priority, provide updated analysis."
      ]

formatTasks :: [Task] -> Text
formatTasks [] = "No existing tasks."
formatTasks tasks = T.unlines $ map formatTask tasks

formatTask :: Task -> Text
formatTask task = "- [" <> prioText <> "] " <> taskTitle task <> ": " <> taskDescription task
  where
    prioText = case taskPriority task of
      Critical -> "CRITICAL"
      High     -> "HIGH"
      Medium   -> "MEDIUM"
      Low      -> "LOW"
