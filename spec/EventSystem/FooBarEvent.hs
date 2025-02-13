module EventSystem.FooBarEvent where

import EventSystem.EventHandler (EventHandler (..))

import "base" Data.Functor.Identity (Identity (..))

data FooBarEvent
  = Foo Int String
  | Bar String Double
  deriving stock (Show)

fooBarIOEventHandler :: EventHandler IO FooBarEvent String
fooBarIOEventHandler = EventHandler $ pure . show

fooBarIdentityEventHandler :: EventHandler Identity FooBarEvent String
fooBarIdentityEventHandler = EventHandler (Identity . show)
