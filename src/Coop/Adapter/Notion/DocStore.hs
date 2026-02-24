module Coop.Adapter.Notion.DocStore
  ( mkLiveDocStoreOps
  ) where

import Coop.Adapter.Notion.Client (notionGet)
import Coop.Adapter.Notion.Types (NotionBlock (..), NotionRichText (..))
import Coop.App.Env (DocStoreOps (..))
import Coop.Domain.Doc (DocId (..), Document (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, parseJSON, withObject, (.:))
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Network.HTTP.Client (Manager)
import GHC.Generics (Generic)

data BlocksResponse = BlocksResponse
  { brResults :: [NotionBlock]
  } deriving stock (Eq, Show, Generic)

instance FromJSON BlocksResponse where
  parseJSON = withObject "BlocksResponse" $ \v ->
    BlocksResponse <$> v .: "results"

mkLiveDocStoreOps :: (MonadIO m) => Text -> Manager -> DocStoreOps m
mkLiveDocStoreOps apiKey manager = DocStoreOps
  { opsGetDocument = \did -> liftIO $ do
      let url = "https://api.notion.com/v1/blocks/" <> unpack (unDocId did) <> "/children?page_size=100"
      putStrLn $ "[DocStore] Fetching document: " <> unpack (unDocId did)
      result <- notionGet manager apiKey url
      case result of
        Left err -> do
          putStrLn $ "[DocStore] Error fetching document " <> unpack (unDocId did) <> ": " <> err
          pure Nothing
        Right resp -> do
          let content = extractText (brResults resp)
          pure $ Just Document { docId = did, docContent = content }
  }

extractText :: [NotionBlock] -> Text
extractText blocks = T.intercalate "\n" $ concatMap blockToText blocks

blockToText :: NotionBlock -> [Text]
blockToText block = map nrtPlainText (nbTexts block)
