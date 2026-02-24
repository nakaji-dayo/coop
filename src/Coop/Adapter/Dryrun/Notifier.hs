module Coop.Adapter.Dryrun.Notifier
  ( mkDryrunNotifierOps
  ) where

import Coop.App.Env (NotifierOps (..))
import Coop.Domain.Notification (Notification (..))
import Katip

mkDryrunNotifierOps :: (KatipContext m) => NotifierOps m
mkDryrunNotifierOps = NotifierOps
  { opsNotify = \notif -> do
      katipAddContext () $ logFM InfoS $ logStr $
        "[DRYRUN] Notification to " <> notifChannel notif <> ": " <> notifText notif

  , opsReplyThread = \channel ts msg -> do
      katipAddContext () $ logFM InfoS $ logStr $
        "[DRYRUN] Reply in " <> channel <> " (thread " <> ts <> "): " <> msg
  }
