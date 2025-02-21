module EventSystem.Transport.InMemory where

import EventSystem.EventHandler (EventHandler (..))
import EventSystem.Transport (Receiver (..), Sender (..))

import "base" Control.Monad.IO.Class (MonadIO (..))
import "base" GHC.Conc (TVar, atomically, newTVarIO, readTVar, writeTVar)

data InMemoryTransport m event a = InMemoryTransport
  { eventQueue :: TVar [event] -- TODO: this should be a more precise data structure. The head is the first element which entered the queue.
  , eventHandler :: EventHandler m event a
  }
  deriving stock (Functor)

instance Sender InMemoryTransport m event a IO () where
  send :: InMemoryTransport m event a -> event -> IO ()
  send (InMemoryTransport queue _) event =
    liftIO . atomically $ do
      events <- readTVar queue
      writeTVar queue (events ++ [event])

instance (MonadIO n) => Receiver InMemoryTransport m event a n where
  receive :: InMemoryTransport m event a -> n [event]
  receive (InMemoryTransport queue _) =
    liftIO . atomically $ do
      events <- readTVar queue
      writeTVar queue []
      pure events

  handler :: InMemoryTransport m event a -> EventHandler m event a
  handler = eventHandler

inMemoryTransport :: EventHandler m event a -> IO (InMemoryTransport m event a)
inMemoryTransport eventHandler =
  InMemoryTransport
    <$> newTVarIO []
    <*> pure eventHandler
