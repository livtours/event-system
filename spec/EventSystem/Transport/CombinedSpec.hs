{-# LANGUAGE GADTs #-}

module EventSystem.Transport.CombinedSpec where

import EventSystem.FooBarEvent

import "event-system" EventSystem.Transport (Receiver (..), Sender (..))
import "event-system" EventSystem.Transport.Combined
import "event-system" EventSystem.Transport.InMemory (inMemoryTransport)

import "hspec" Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Transport.Combined" $ do
  it "processes correctly events with multiple transfers" $ do
    transport1 <- inMemoryTransport fooBarIOEventHandler
    transport2 <- inMemoryTransport fooBarIOEventHandler
    let transport = ConsCombined transport1 (ConsCombined transport2 EmptyCombined)
        (send1 :# (send2 :# HNil)) = send transport (Foo 42 "asdf")
    send1
    send2
    let (ioEvents1 :@ (ioEvents2 :@ HContextNil)) = receive transport
    events1 <- ioEvents1
    events2 <- ioEvents2
    events1 `shouldBe` [Foo 42 "asdf"]
    events2 `shouldBe` [Foo 42 "asdf"]
