module EventSystem.Transport.InMemory where

import EventSystem.EventHandler (EventHandler (..))
import EventSystem.Transport (Sender (..))

import "base" Control.Monad.IO.Class (MonadIO (..))
import "base" Data.Traversable (for)
import "base" GHC.Conc (TVar, atomically, newTVarIO, readTVar, writeTVar)

data InMemoryTransport m event a = InMemoryTransport
  { eventQueue :: TVar [event] -- TODO: this should be a more precise data structure. The head is the first element which entered the queue.
  , eventHandler :: EventHandler m event a
  }
  deriving stock (Functor)

instance (MonadIO m) => Sender m event () (InMemoryTransport m event a) where
  send :: InMemoryTransport m event a -> event -> m ()
  send (InMemoryTransport queue _) event =
    liftIO . atomically $ do
      events <- readTVar queue
      writeTVar queue (events ++ [event])

process :: (MonadIO m, Monoid a) => InMemoryTransport m event a -> m a
process (InMemoryTransport queue (EventHandler handler)) = do
  -- pull events from the queue
  -- TODO: add a mechanism for ackowledging and retrying?
  events <- liftIO . atomically $ do
    events <- readTVar queue
    writeTVar queue []
    pure events
  mconcat <$> for events handler

inMemoryTransport :: EventHandler m event a -> IO (InMemoryTransport m event a)
inMemoryTransport eventHandler =
  InMemoryTransport
    <$> newTVarIO []
    <*> pure eventHandler
