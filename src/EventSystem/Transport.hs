{-# LANGUAGE FunctionalDependencies #-}

module EventSystem.Transport where

-- | A `Sender` receives an event and processes it immediately
--
-- Processing could mean immediately invoking some `EventHandler`s
-- or storing the event somewhere so that it can be processed later on
--
-- NOTE: This says a Sender can be turned into  a Handler
class Sender m event a sender | sender -> m event a where
  send :: sender -> event -> m a

-- class Receiver m event where
--   receive :: m event

-- class (Sender m event, Receiver m event) => Transport m event
