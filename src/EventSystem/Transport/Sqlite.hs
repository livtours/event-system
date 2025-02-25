{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module EventSystem.Transport.Sqlite where

import EventSystem.EventHandler (EventHandler (..))
import EventSystem.Transport (Receiver (..), Sender (..))

import "aeson" Data.Aeson (FromJSON, ToJSON, decode, encode)
import "base" Control.Monad (MonadPlus (mzero))
import "bytestring" Data.ByteString (toStrict)
import "sqlite-simple" Database.SQLite.Simple (FromRow (..), SQLData (..), ToRow (..), execute, execute_, query_, withConnection)
import "sqlite-simple" Database.SQLite.Simple.FromRow (RowParser, field)

-- | A `SqliteTransport` stores the event, maybe unsurprisingly, in a table in a `SQLite` database, so that they can be processed later
data SqliteTransport m event a = SqliteTransport
  { sqliteConnectionString :: String
  , eventHandler :: EventHandler m event a
  }
  deriving stock (Functor)

-- | newtype used to define `ToRow` and `FromRow` instances for events
newtype RawEvent event = RawEvent {refined :: event}

instance (ToJSON event) => ToRow (RawEvent event) where
  toRow :: RawEvent event -> [SQLData]
  toRow (RawEvent event) = [SQLBlob . toStrict . encode $ event]

instance (FromJSON event) => FromRow (RawEvent event) where
  fromRow :: RowParser (RawEvent event)
  fromRow = do
    content <- field
    case decode content of
      Nothing -> mzero
      Just event -> pure $ RawEvent event

-- | Sending a message with the `SqliteTransport` stores it in the @events@ table of a SQLite database identified by the `connectionString`.
-- The operation happens in the `IO` monad
instance (ToJSON event) => Sender SqliteTransport m event a where
  type SendResult SqliteTransport m a = IO ()
  send :: SqliteTransport m event a -> event -> IO ()
  send (SqliteTransport connectionString _) event =
    withConnection connectionString $
      \connection -> do
        execute_
          connection
          "CREATE TABLE IF NOT EXISTS events (event BLOB NOT NULL);"
        execute
          connection
          "INSERT INTO events (event) VALUES (?)"
          (RawEvent event)

-- | Receiving a message with the `SqliteTransport` queries the @events@ table, retrieves all the rows and then empties it
-- The operation happens in the `IO` monad
instance (FromJSON event) => Receiver SqliteTransport m event a where
  type ReceiverContext SqliteTransport m = IO
  type HandlerResult SqliteTransport a = a
  receive :: SqliteTransport m event a -> IO [event]
  receive (SqliteTransport connectionString _) =
    withConnection connectionString $
      \connection -> do
        execute_
          connection
          "CREATE TABLE IF NOT EXISTS events (event BLOB NOT NULL);"
        rawEvents <-
          query_
            connection
            "SELECT event FROM events"
        execute_
          connection
          "DELETE FROM events"
        pure $ refined <$> rawEvents

  handler :: SqliteTransport m event a -> EventHandler m event a
  handler = eventHandler
