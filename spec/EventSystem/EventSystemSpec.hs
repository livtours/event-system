module EventSystem.EventSystemSpec where

import "event-system" EventSystem.EventSystem (EventSystem (..), dispatch)

import "base" Data.Functor.Identity (Identity (..))
import "hspec" Test.Hspec (Spec, describe, it, shouldBe)

data FooBarEvent
  = Foo Int String
  | Bar String Double
  deriving stock (Show)

fooBarEventSystem :: EventSystem Identity FooBarEvent String
fooBarEventSystem = EventSystem (Identity . show)

spec :: Spec
spec = describe "Event System" $ do
  describe "dispatch" $ do
    it "executes the listeners" $ do
      dispatch fooBarEventSystem (Foo 23 "abc") `shouldBe` pure "Foo 23 \"abc\""
