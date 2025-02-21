{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}

module EventSystem.Transport where

import Data.Traversable (for)
import EventSystem.EventHandler (EventHandler (..))

-- | A `Sender` receives an event and processes it immediately
--
-- Processing could mean immediately invoking some `EventHandler`s
-- or storing the event somewhere so that it can be processed later on
--
-- @transport@ is the `Transport` itself
-- @m@ is the context in which the handler of the `Transport` operates
-- @event@ is the type of events which can be sent by the `Sender`
-- @a@ is the type of the result of the handler of the `Transport`
-- @b@ is the potential result of the `send` operation
class Sender transport m event a n b | transport a -> b where
  send :: transport m event a -> event -> n b

-- | A `Receiver` is able to retrieve events and process them
--
-- @transport@ is the `Transport` itself
-- @m@ is the context in which the handler of the `Transport` operates
-- @event@ is the type of events which can be sent by the `Sender`
-- @a@ is the type of the result of the handler of the `Transport`
-- @n@ is the context in which the events are retrieved
class Receiver transport m event a n where
  receive :: transport m event a -> n [event]
  handler :: transport m event a -> EventHandler m event a

process :: forall transport m event a. (Receiver transport m event a m, Monad m, Monoid a) => transport m event a -> m a
process receiver = do
  events <- receive receiver
  mconcat <$> for events (dispatch $ handler @transport @m @event @a @m receiver)
