module Coop.Effect.DocStore
  ( DocStore (..)
  ) where

import Coop.Domain.Doc (DocId, Document)

class (Monad m) => DocStore m where
  getDocument :: DocId -> m (Maybe Document)
