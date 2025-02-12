module EventSystem.EventHandlerSpec where

import "event-system" EventSystem.EventHandler (EventHandler (..), dispatch)

import "base" Control.Arrow (Arrow (..))
import "base" Data.Functor.Identity (Identity (..))
import "hspec" Test.Hspec (Spec, describe, it, shouldBe)

data FooBarEvent
  = Foo Int String
  | Bar String Double
  deriving stock (Show)

fooBarEventHandler :: EventHandler Identity FooBarEvent String
fooBarEventHandler = EventHandler (Identity . show)

spec :: Spec
spec = describe "Event System" $ do
  describe "dispatch" $ do
    it "executes the listeners" $ do
      dispatch fooBarEventHandler (Foo 23 "abc") `shouldBe` pure "Foo 23 \"abc\""

    it "executes multiple listeners" $ do
      dispatch (fooBarEventHandler &&& fooBarEventHandler) (Foo 23 "abc") `shouldBe` pure ("Foo 23 \"abc\"", "Foo 23 \"abc\"")
