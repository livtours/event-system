{-# LANGUAGE TemplateHaskell #-}

module EventSystem.FooBarEvent where

import EventSystem.EventHandler (EventHandler (..))

import "aeson" Data.Aeson.TH (defaultOptions, deriveJSON)
import "base" Data.Functor.Identity (Identity (..))

data FooBarEvent
  = Foo Int String
  | Bar String Double
  deriving stock (Eq, Show)

$(deriveJSON defaultOptions ''FooBarEvent)

fooBarIOEventHandler :: EventHandler IO FooBarEvent String
fooBarIOEventHandler = EventHandler $ pure . show

fooBarIdentityEventHandler :: EventHandler Identity FooBarEvent String
fooBarIdentityEventHandler = EventHandler (Identity . show)
