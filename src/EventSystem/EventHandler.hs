{-# LANGUAGE TypeFamilies #-}

module EventSystem.EventHandler where

import "base" Control.Applicative (Alternative (..))
import "base" Control.Arrow (Arrow (..))
import "base" Control.Category (Category (..))
import "base" Control.Monad (MonadPlus (..), (>=>))
import "base" Data.Functor.Contravariant (Contravariant (..))
import "distributive" Data.Distributive (Distributive (..))
import "profunctors" Data.Profunctor (Choice (..), Closed (..), Cochoice (..), Mapping (..), Profunctor (..), Strong (..))
import "profunctors" Data.Profunctor.Rep (Representable (..))
import "profunctors" Data.Profunctor.Sieve (Sieve (..))
import "profunctors" Data.Profunctor.Traversing (Traversing (..))
import "profunctors" Data.Profunctor.Unsafe (Profunctor (..))
import "base" Prelude hiding (id, (.))

-- | An `EventHandler` is nothing else but a function which takes an event an produces some effects returning a result
--
-- The function could be extremely complicated and defined in terms of the aggregation of many simpler `EventHandler`s
newtype EventHandler m event a = EventHandler {dispatch :: event -> m a}

instance (Functor m) => Profunctor (EventHandler m) where
  dimap :: (a -> b) -> (c -> d) -> EventHandler m b c -> EventHandler m a d
  dimap ab cd (EventHandler bfc) = EventHandler (fmap cd . bfc . ab)
  {-# INLINE dimap #-}

  lmap :: (a -> b) -> EventHandler m b c -> EventHandler m a c
  lmap k (EventHandler f) = EventHandler (f . k)
  {-# INLINE lmap #-}

  rmap :: (b -> c) -> EventHandler m a b -> EventHandler m a c
  rmap k (EventHandler f) = EventHandler (fmap k . f)
  {-# INLINE rmap #-}

instance (Functor m) => Strong (EventHandler m) where
  first' :: EventHandler m a b -> EventHandler m (a, c) (b, c)
  first' (EventHandler f) = EventHandler $ \ ~(a, c) -> (,c) <$> f a
  {-# INLINE first' #-}

  second' :: EventHandler m a b -> EventHandler m (c, a) (c, b)
  second' (EventHandler f) = EventHandler $ \ ~(c, a) -> (,) c <$> f a
  {-# INLINE second' #-}

instance (Distributive f) => Closed (EventHandler f) where
  closed :: EventHandler f a b -> EventHandler f (x -> a) (x -> b)
  closed (EventHandler afb) = EventHandler $ \xa -> distribute $ \x -> afb (xa x)

instance (Traversable f) => Cochoice (EventHandler f) where
  unright :: EventHandler f (Either d a) (Either d b) -> EventHandler f a b
  unright (EventHandler f) = EventHandler (go . Right)
    where
      go = either (go . Left) id . sequence . f

instance (Applicative f) => Choice (EventHandler f) where
  left' :: EventHandler f a b -> EventHandler f (Either a c) (Either b c)
  left' (EventHandler f) = EventHandler $ either (fmap Left . f) (pure . Right)
  {-# INLINE left' #-}

  right' :: EventHandler f a b -> EventHandler f (Either c a) (Either c b)
  right' (EventHandler f) = EventHandler $ either (pure . Left) (fmap Right . f)
  {-# INLINE right' #-}

instance (Applicative m) => Traversing (EventHandler m) where
  traverse' :: (Traversable f) => EventHandler m a b -> EventHandler m (f a) (f b)
  traverse' (EventHandler m) = EventHandler (traverse m)

  wander :: (forall f. (Applicative f) => (a -> f b) -> s -> f t) -> EventHandler m a b -> EventHandler m s t
  wander f (EventHandler amb) = EventHandler (f amb)

instance (Applicative m, Distributive m) => Mapping (EventHandler m) where
  map' :: (Functor f) => EventHandler m a b -> EventHandler m (f a) (f b)
  map' (EventHandler f) = EventHandler (collect f)

  roam :: ((a -> b) -> s -> t) -> EventHandler m a b -> EventHandler m s t
  roam f = EventHandler #. genMap f .# dispatch
    where
      genMap :: (Distributive f) => ((a -> b) -> s -> t) -> (a -> f b) -> s -> f t
      genMap abst afb s = fmap (`abst` s) (distribute afb)

instance (Functor f) => Representable (EventHandler f) where
  type Rep (EventHandler f) = f

  tabulate :: (d -> Rep (EventHandler f) c) -> EventHandler f d c
  tabulate = EventHandler
  {-# INLINE tabulate #-}

instance (Functor f) => Sieve (EventHandler f) f where
  sieve :: EventHandler f a b -> a -> f b
  sieve = dispatch
  {-# INLINE sieve #-}

instance (Functor f) => Functor (EventHandler f a) where
  fmap :: (a1 -> b) -> EventHandler f a a1 -> EventHandler f a b
  fmap = rmap
  {-# INLINE fmap #-}

instance (Applicative f) => Applicative (EventHandler f a) where
  pure :: a1 -> EventHandler f a a1
  pure a = EventHandler $ \_ -> pure a

  (<*>) :: EventHandler f a (a1 -> b) -> EventHandler f a a1 -> EventHandler f a b
  EventHandler ff <*> EventHandler fx = EventHandler $ \a -> ff a <*> fx a

  (*>) :: EventHandler f a a1 -> EventHandler f a b -> EventHandler f a b
  EventHandler ff *> EventHandler fx = EventHandler $ \a -> ff a *> fx a

  (<*) :: EventHandler f a a1 -> EventHandler f a b -> EventHandler f a a1
  EventHandler ff <* EventHandler fx = EventHandler $ \a -> ff a <* fx a

instance (Alternative f) => Alternative (EventHandler f a) where
  empty :: EventHandler f a a1
  empty = EventHandler $ const empty

  (<|>) :: EventHandler f a a1 -> EventHandler f a a1 -> EventHandler f a a1
  EventHandler f <|> EventHandler g = EventHandler $ \a -> f a <|> g a

instance (Monad f) => Monad (EventHandler f a) where
  (>>=) :: EventHandler f a a1 -> (a1 -> EventHandler f a b) -> EventHandler f a b
  EventHandler m >>= f = EventHandler $ \e -> do
    a <- m e
    dispatch (f a) e

instance (MonadPlus f) => MonadPlus (EventHandler f a) where
  mzero :: EventHandler f a a1
  mzero = EventHandler $ const mzero

  mplus :: EventHandler f a a1 -> EventHandler f a a1 -> EventHandler f a a1
  EventHandler f `mplus` EventHandler g = EventHandler $ \a -> f a `mplus` g a

instance (Distributive f) => Distributive (EventHandler f a) where
  distribute :: (Functor f1) => f1 (EventHandler f a a1) -> EventHandler f a (f1 a1)
  distribute fs = EventHandler $ \a -> collect (($ a) .# dispatch) fs

instance (Monad f) => Category (EventHandler f) where
  id :: EventHandler f a a
  id = EventHandler return

  (.) :: EventHandler f b c -> EventHandler f a b -> EventHandler f a c
  EventHandler f . EventHandler g = EventHandler $ g >=> f

instance (Contravariant f) => Contravariant (EventHandler f a) where
  contramap :: (a' -> a1) -> EventHandler f a a1 -> EventHandler f a a'
  contramap f (EventHandler g) = EventHandler (contramap f . g)
  {-# INLINE contramap #-}

instance (Monad m) => Arrow (EventHandler m) where
  arr :: (b -> c) -> EventHandler m b c
  arr f = EventHandler (return . f)

  first :: EventHandler m b c -> EventHandler m (b, d) (c, d)
  first (EventHandler f) = EventHandler (\ ~(b, d) -> f b >>= \c -> return (c, d))

  second :: EventHandler m b c -> EventHandler m (d, b) (d, c)
  second (EventHandler f) = EventHandler (\ ~(d, b) -> f b >>= \c -> return (d, c))
