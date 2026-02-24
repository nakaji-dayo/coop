module Coop.Effect.Notifier
  ( Notifier (..)
  ) where

import Data.Text (Text)
import Coop.Domain.Notification (Notification)

class (Monad m) => Notifier m where
  notify      :: Notification -> m ()
  replyThread :: Text -> Text -> Text -> m ()
