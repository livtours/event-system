module EventSystem.Transport.Sync where

import EventSystem.EventHandler (EventHandler (..))
import EventSystem.Transport (Sender (..))

-- | A `SyncTransport` just wraps an `EventHandler` which is immediately executed whenever an event is received
newtype SyncTransport m event a = SyncTransport (EventHandler m event a)

instance Sender SyncTransport m event a m a where
  send :: SyncTransport m event a -> event -> m a
  send (SyncTransport (EventHandler eventHandler)) = eventHandler
