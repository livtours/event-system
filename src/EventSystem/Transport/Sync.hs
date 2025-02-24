{-# LANGUAGE TypeFamilies #-}

module EventSystem.Transport.Sync where

import EventSystem.EventHandler (EventHandler (..))
import EventSystem.Transport (Sender (..))

import "base" Data.Kind (Type)

-- | A `SyncTransport` just wraps an `EventHandler` which is immediately executed whenever an event is received
newtype SyncTransport m event a = SyncTransport (EventHandler m event a)

-- | A `SyncTransport` can just send events and produce a result according to its `EventHandler`
instance Sender SyncTransport m event (a :: Type) where
  type SendResult SyncTransport a = a
  type SendContext SyncTransport m = m
  send :: SyncTransport m event a -> event -> m a
  send (SyncTransport (EventHandler eventHandler)) = eventHandler
