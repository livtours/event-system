{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module EventSystem.Transport.Combined where

import EventSystem.EventHandler (EventHandler (..))
import EventSystem.Transport (Receiver (..), Sender (..))

import "base" Data.Kind (Constraint, Type)

data Combined (transports :: [(Type -> Type) -> Type -> Type -> Type]) m event as where
  EmptyCombined :: Combined '[] m event '[]
  ConsCombined :: transport m event a -> Combined transports m event as -> Combined (transport ': transports) m event (a ': as)

-- | Heterogeneous lists
data HList types where
  HNil :: HList '[]
  (:#) :: t -> HList ts -> HList (t ': ts)

-- | Heterogeneous contexts
data HContext (contexts :: [Type -> Type]) a where
  HContextNil :: HContext '[] a
  (:@) :: c a -> HContext contexts a -> HContext (c ': contexts) a

-- Sender

type family AllSender transports m event as :: Constraint where
  AllSender '[] _ _ _ = ()
  AllSender _ _ _ '[] = ()
  AllSender (transport ': transports) m event (a ': as) = (Sender transport m event a, AllSender transports m event as)

type family AllSendResults transports m as where
  AllSendResults '[] _ _ = '[]
  AllSendResults _ _ '[] = '[]
  AllSendResults (transport ': transports) m (a ': as) = SendResult transport m a ': AllSendResults transports m as

instance (AllSender transports m event as) => Sender (Combined transports) m event as where
  type SendResult (Combined transports) m as = HList (AllSendResults transports m as)
  send :: Combined transports m event as -> event -> HList (AllSendResults transports m as)
  send EmptyCombined _ = HNil
  send (ConsCombined transport combined) event = send transport event :# send combined event

-- Receiver

type family AllReceiver transports m event as :: Constraint where
  AllReceiver '[] _ _ _ = ()
  AllReceiver _ _ _ '[] = ()
  AllReceiver (transport ': transports) m event (a ': as) = (Receiver transport m event a, AllReceiver transports m event as)

type family AllReceiverContexts transports m :: [Type -> Type] where
  AllReceiverContexts '[] _ = '[]
  AllReceiverContexts (transport ': transports) m = ReceiverContext transport m ': AllReceiverContexts transports m

type family AllHandlerResults transports as :: [Type] where
  AllHandlerResults '[] _ = '[]
  AllHandlerResults _ '[] = '[]
  AllHandlerResults (transport ': transports) (a ': as) = HandlerResult transport a ': AllHandlerResults transports as

instance (Applicative m, AllReceiver transports m event as) => Receiver (Combined transports) m event as where
  type ReceiverContext (Combined transports) m = HContext (AllReceiverContexts transports m)
  type HandlerResult (Combined transports) as = HList (AllHandlerResults transports as)

  receive :: (Combined transports) m event as -> HContext (AllReceiverContexts transports m) [event]
  receive EmptyCombined = HContextNil
  receive (ConsCombined transport combined) = receive transport :@ receive combined

  handler :: (Combined transports) m event as -> EventHandler m event (HandlerResult (Combined transports) as)
  handler EmptyCombined = EventHandler $ \_ -> pure HNil
  handler (ConsCombined transport combined) = EventHandler $ \event -> (:#) <$> dispatch (handler transport) event <*> dispatch (handler combined) event
