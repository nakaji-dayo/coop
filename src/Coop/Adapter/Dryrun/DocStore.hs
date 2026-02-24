module Coop.Adapter.Dryrun.DocStore
  ( mkDryrunDocStoreOps
  ) where

import Coop.App.Env (DocStoreOps (..))
import Coop.Domain.Doc (DocId (..), Document (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text, unpack)
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist)

mkDryrunDocStoreOps :: (MonadIO m) => Text -> DocStoreOps m
mkDryrunDocStoreOps dataDir = DocStoreOps
  { opsGetDocument = \did -> liftIO $ do
      let path = unpack dataDir <> "/" <> unpack (unDocId did) <> ".md"
      exists <- doesFileExist path
      if exists
        then do
          content <- TIO.readFile path
          pure $ Just Document { docId = did, docContent = content }
        else
          pure Nothing
  }
