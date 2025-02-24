{-# LANGUAGE TypeFamilies #-}

module EventSystem.Transport.Forked where

import EventSystem.EventHandler (EventHandler (..))
import EventSystem.Transport (Sender (..))

import "base" Control.Concurrent (ThreadId)
import "lifted-base" Control.Concurrent.Lifted (fork)
import "monad-control" Control.Monad.Trans.Control (MonadBaseControl)

-- | A `ForkedTransport` wraps an event handler, which is immediately executed, in another thread, whenever an event is received
newtype ForkedTransport m event a = ForkedTransport (EventHandler m event a)

instance (MonadBaseControl IO m) => Sender ForkedTransport m event () where
  type SendResult ForkedTransport () = ThreadId
  type SendContext ForkedTransport m = m
  send :: ForkedTransport m event () -> event -> m ThreadId
  send (ForkedTransport (EventHandler eventHandler)) event =
    fork $ eventHandler event
