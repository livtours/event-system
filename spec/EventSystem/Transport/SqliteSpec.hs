module EventSystem.Transport.SqliteSpec where

import EventSystem.FooBarEvent

import "event-system" EventSystem.Transport (Sender (..), process)
import "event-system" EventSystem.Transport.Sqlite (SqliteTransport (..))

import "base" Control.Exception (bracket_)
import "base" Data.List (singleton)
import "directory" System.Directory (removeFile)
import "hspec" Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Transport.Sqlite" $ do
  let dbFile = "test.db"
  it "processes correctly events after storing them in a temporary database" $ do
    bracket_
      (writeFile dbFile "")
      (removeFile dbFile)
      $ do
        let transport = SqliteTransport dbFile fooBarIOEventHandler
        send transport (Foo 42 "asdf")
        send transport (Bar "qwer" 12.34)
        results <- process $ singleton <$> transport
        results `shouldBe` ["Foo 42 \"asdf\"", "Bar \"qwer\" 12.34"]
