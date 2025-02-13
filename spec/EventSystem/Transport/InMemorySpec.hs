module EventSystem.Transport.InMemorySpec where

import EventSystem.FooBarEvent

import "event-system" EventSystem.Transport (Sender (..))
import "event-system" EventSystem.Transport.InMemory (inMemoryTransport, process)

import "base" Data.List (singleton)
import "hspec" Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Transport.InMemory" $ do
  it "processes correctly events after storing them in a queue" $ do
    transport <- inMemoryTransport fooBarIOEventHandler
    send transport (Foo 42 "asdf")
    send transport (Bar "qwer" 12.34)
    results <- process $ singleton <$> transport
    results `shouldBe` ["Foo 42 \"asdf\"", "Bar \"qwer\" 12.34"]
