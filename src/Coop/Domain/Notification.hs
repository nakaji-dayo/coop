module Coop.Domain.Notification
  ( Notification (..)
  , NotificationLevel (..)
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)

data NotificationLevel = Info | Warning | Urgent
  deriving stock (Eq, Show, Generic)

data Notification = Notification
  { notifChannel :: Text
  , notifText    :: Text
  , notifLevel   :: NotificationLevel
  } deriving stock (Eq, Show, Generic)
