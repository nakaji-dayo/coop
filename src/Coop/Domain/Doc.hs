module Coop.Domain.Doc
  ( DocId (..)
  , Document (..)
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)

newtype DocId = DocId { unDocId :: Text }
  deriving stock (Eq, Ord, Show, Generic)

data Document = Document
  { docId      :: DocId
  , docContent :: Text
  } deriving stock (Eq, Show, Generic)
