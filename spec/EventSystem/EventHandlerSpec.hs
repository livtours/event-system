module EventSystem.EventHandlerSpec where

import EventSystem.FooBarEvent

import "event-system" EventSystem.EventHandler (EventHandler (..), dispatch)

import "base" Control.Arrow (Arrow (..))
import "hspec" Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Event System" $ do
  describe "dispatch" $ do
    it "executes the listeners" $ do
      dispatch fooBarIdentityEventHandler (Foo 23 "abc") `shouldBe` pure "Foo 23 \"abc\""

    it "executes multiple listeners" $ do
      dispatch (fooBarIdentityEventHandler &&& fooBarIdentityEventHandler) (Foo 23 "abc") `shouldBe` pure ("Foo 23 \"abc\"", "Foo 23 \"abc\"")
