{-# LANGUAGE OverloadedStrings #-}

module EventSystem.Transport.Sqlite where

import EventSystem.EventHandler (EventHandler (..))
import EventSystem.Transport (Receiver (..), Sender (..))

import "aeson" Data.Aeson (FromJSON, ToJSON, decode, encode)
import "base" Control.Monad (MonadPlus (mzero))
import "base" Control.Monad.IO.Class (MonadIO (..))
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

instance (ToJSON event, MonadIO n) => Sender SqliteTransport m event a n () where
  send :: SqliteTransport m event a -> event -> n ()
  send (SqliteTransport connectionString _) event =
    liftIO $
      withConnection connectionString $
        \connection -> do
          execute_
            connection
            "CREATE TABLE IF NOT EXISTS events (event BLOB NOT NULL);"
          execute
            connection
            "INSERT INTO events (event) VALUES (?)"
            (RawEvent event)

instance (FromJSON event, MonadIO n) => Receiver SqliteTransport m event a n where
  receive :: SqliteTransport m event a -> n [event]
  receive (SqliteTransport connectionString _) =
    liftIO . withConnection connectionString $
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
