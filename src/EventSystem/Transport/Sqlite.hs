{-# LANGUAGE OverloadedStrings #-}

module EventSystem.Transport.Sqlite where

import EventSystem.EventHandler (EventHandler (..))
import EventSystem.Transport (Sender (..))

import "aeson" Data.Aeson (FromJSON, ToJSON, decode, encode)
import "base" Control.Monad (MonadPlus (mzero))
import "base" Control.Monad.IO.Class (MonadIO (..))
import "base" Data.Traversable (for)
import "bytestring" Data.ByteString (toStrict)
import "sqlite-simple" Database.SQLite.Simple (FromRow (..), SQLData (..), ToRow (..), execute, execute_, query_, withConnection)
import "sqlite-simple" Database.SQLite.Simple.FromRow (RowParser, field)

data SqliteTransport m event a = SqliteTransport
  { sqliteConnectionString :: String
  , eventHandler :: EventHandler m event a
  }
  deriving stock (Functor)

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

instance (MonadIO m, ToJSON event) => Sender m event () (SqliteTransport m event a) where
  send :: SqliteTransport m event a -> event -> m ()
  send (SqliteTransport connectionString _) event =
    liftIO . withConnection connectionString $
      \connection -> do
        execute_
          connection
          "CREATE TABLE IF NOT EXISTS events (event BLOB NOT NULL);"
        execute
          connection
          "INSERT INTO events (event) VALUES (?)"
          (RawEvent event)

process :: (MonadIO m, Monoid a, FromJSON event) => SqliteTransport m event a -> m a
process (SqliteTransport connectionString (EventHandler handler)) = do
  -- pull events from the database
  -- TODO: add a mechanism for ackowledging and retrying?
  events <- liftIO . withConnection connectionString $
    \connection -> do
      execute_
        connection
        "CREATE TABLE IF NOT EXISTS events (event BLOB NOT NULL);"
      rawEvents <-
        query_
          connection
          "SELECT event FROM events"
      pure $ refined <$> rawEvents
  mconcat <$> for events handler
