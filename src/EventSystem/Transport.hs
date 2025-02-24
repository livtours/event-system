{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}

module EventSystem.Transport where

import EventSystem.EventHandler (EventHandler (..))

import "base" Data.Kind (Type)
import "base" Data.Traversable (for)

-- | A `Sender` receives an event and processes it immediately
--
-- Processing could mean immediately invoking some `EventHandler`s
-- or storing the event somewhere so that it can be processed later on
--
-- @transport@ is the `Transport` itself
-- @m@ is the context in which the handler of the `Transport` operates
-- @event@ is the type of events which can be sent by the `Sender`
class Sender transport m event a where
  type SendContext transport m :: Type -> Type
  type SendResult transport a
  send :: transport m event a -> event -> (SendContext transport m) (SendResult transport a)

-- | A `Receiver` is able to retrieve events and process them
--
-- @transport@ is the `Transport` itself
-- @m@ is the context in which the handler of the `Transport` operates
-- @event@ is the type of events which can be sent by the `Sender`
-- @a@ is the type of the result of the handler of the `Transport`
class Receiver transport m event a where
  type ReceiverContext transport m :: Type -> Type
  receive :: transport m event a -> (ReceiverContext transport m) [event]
  handler :: transport m event a -> EventHandler m event a

process :: forall transport m event a. (ReceiverContext transport m ~ m, Receiver transport m event a, Monad m, Monoid a) => transport m event a -> m a
process receiver = do
  events <- receive receiver
  mconcat <$> for events (dispatch $ handler @transport @m @event @a receiver)
