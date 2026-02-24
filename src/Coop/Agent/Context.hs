module Coop.Agent.Context
  ( AgentContext (..)
  , buildContext
  ) where

import Coop.App.Env (Env (..))
import Coop.App.Log (logDebug)
import Coop.Config (Config (..), NotionConfig (..))
import Coop.Domain.Doc (DocId (..), Document (..))
import Coop.Domain.Task (Task)
import Coop.Effect.DocStore (DocStore (..))
import Coop.Effect.TaskStore (TaskStore (..))
import Control.Monad.Reader (MonadReader, asks)
import Data.Text (Text)
import Katip (KatipContext)

data AgentContext = AgentContext
  { acGuidelines   :: Text
  , acInstructions :: Text
  , acExistingTasks :: [Task]
  } deriving stock (Show)

buildContext
  :: (DocStore m, TaskStore m, MonadReader (Env m) m, KatipContext m)
  => m AgentContext
buildContext = do
  config <- asks envConfig
  let notionCfg = cfgNotion config
      guidelinesId = DocId (notionGuidelinesPageId notionCfg)
      instructionsId = DocId (notionInstructionsPageId notionCfg)

  guidelinesDoc <- getDocument guidelinesId
  instructionsDoc <- getDocument instructionsId
  tasks <- listTasks

  let guidelines = maybe "No guidelines available." docContent guidelinesDoc
      instructions = maybe "No instructions available." docContent instructionsDoc

  logDebug $ "Guidelines: " <> guidelines
  logDebug $ "Instructions: " <> instructions

  pure AgentContext
    { acGuidelines   = guidelines
    , acInstructions = instructions
    , acExistingTasks = tasks
    }
