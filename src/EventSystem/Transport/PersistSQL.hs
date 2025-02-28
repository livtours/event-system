{-# LANGUAGE TypeFamilies #-}

module EventSystem.Transport.PersistSQL where

import EventSystem.EventHandler (EventHandler (..))
import EventSystem.Transport (Receiver (..), Sender (..))

import "persistent" Database.Persist.Sql (BackendCompatible, Entity (..), PersistEntity (..), PersistQueryRead, PersistQueryWrite (..), PersistRecordBackend, PersistStoreWrite (..), SafeToInsert, SqlBackend, runSqlPool, selectList)
import "resource-pool" Data.Pool (Pool)

-- | A `PersistSQLTransport` stores the event in a SQL table,
-- using Persistent as a database abstraction library
-- and requiring @event@ to be an @Entity@
data PersistSQLTransport backend m event a = PersistSQLTransport
  { connectionPool :: Pool backend
  , eventHandler :: EventHandler m event a
  }

-- | Sending a message with the `PersistSQLTransport` stores it in the database table identified by the @event@ entity
-- The operation happens in the `IO` monad
instance (PersistRecordBackend event backend, BackendCompatible SqlBackend backend, PersistStoreWrite backend, SafeToInsert event) => Sender (PersistSQLTransport backend) m event (Key event) where
  type SendResult (PersistSQLTransport backend) m (Key event) = IO (Key event)
  send :: PersistSQLTransport backend m event (Key a) -> event -> IO (Key event)
  send (PersistSQLTransport connectionPool _) event = do
    runSqlPool
      (insert event)
      connectionPool

-- | Receiving messages with the `PersistSQLTransport` queries the table associated to the @event@ entity, retrieves all the rows and then empties it
-- The operation happens in the `IO` monad
instance (PersistRecordBackend event backend, BackendCompatible SqlBackend backend, PersistQueryRead backend, PersistQueryWrite backend) => Receiver (PersistSQLTransport backend) m event a where
  type ReceiverContext (PersistSQLTransport backend) m = IO
  type HandlerResult (PersistSQLTransport backend) a = a

  receive :: PersistSQLTransport backend m event a -> IO [event]
  receive (PersistSQLTransport connectionPool _) = do
    runSqlPool
      ( do
          events <- selectList [] []
          deleteWhere @_ @_ @event []
          pure $ entityVal <$> events
      )
      connectionPool

  handler :: PersistSQLTransport backend m event a -> EventHandler m event a
  handler = eventHandler
