module EventSystem.Transport.SyncSpec where

import EventSystem.FooBarEvent

import "event-system" EventSystem.Transport (Sender (..))
import "event-system" EventSystem.Transport.Sync (SyncTransport (..))

import "hspec" Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "EventSystem.Transport.Sync" $ do
  it "immediately executes the handler" $ do
    let transport = SyncTransport fooBarIOEventHandler
    result <- send transport (Foo 42 "asdf")
    result `shouldBe` "Foo 42 \"asdf\""
