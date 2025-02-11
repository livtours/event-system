{-# LANGUAGE TypeFamilies #-}

module EventSystem.EventSystem where

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

-- | An event system is nothing else but a function which takes an event an produces some effects returning a result
--
-- The function could be extremely complicated and defined in terms of the aggregation of many simpler listeners
newtype EventSystem m event a = EventSystem {dispatch :: event -> m a}

instance (Functor m) => Profunctor (EventSystem m) where
  dimap :: (a -> b) -> (c -> d) -> EventSystem m b c -> EventSystem m a d
  dimap ab cd (EventSystem bfc) = EventSystem (fmap cd . bfc . ab)
  {-# INLINE dimap #-}

  lmap :: (a -> b) -> EventSystem m b c -> EventSystem m a c
  lmap k (EventSystem f) = EventSystem (f . k)
  {-# INLINE lmap #-}

  rmap :: (b -> c) -> EventSystem m a b -> EventSystem m a c
  rmap k (EventSystem f) = EventSystem (fmap k . f)
  {-# INLINE rmap #-}

instance (Functor m) => Strong (EventSystem m) where
  first' :: EventSystem m a b -> EventSystem m (a, c) (b, c)
  first' (EventSystem f) = EventSystem $ \ ~(a, c) -> (,c) <$> f a
  {-# INLINE first' #-}

  second' :: EventSystem m a b -> EventSystem m (c, a) (c, b)
  second' (EventSystem f) = EventSystem $ \ ~(c, a) -> (,) c <$> f a
  {-# INLINE second' #-}

instance (Distributive f) => Closed (EventSystem f) where
  closed :: EventSystem f a b -> EventSystem f (x -> a) (x -> b)
  closed (EventSystem afb) = EventSystem $ \xa -> distribute $ \x -> afb (xa x)

instance (Traversable f) => Cochoice (EventSystem f) where
  unright :: EventSystem f (Either d a) (Either d b) -> EventSystem f a b
  unright (EventSystem f) = EventSystem (go . Right)
    where
      go = either (go . Left) id . sequence . f

instance (Applicative f) => Choice (EventSystem f) where
  left' :: EventSystem f a b -> EventSystem f (Either a c) (Either b c)
  left' (EventSystem f) = EventSystem $ either (fmap Left . f) (pure . Right)
  {-# INLINE left' #-}

  right' :: EventSystem f a b -> EventSystem f (Either c a) (Either c b)
  right' (EventSystem f) = EventSystem $ either (pure . Left) (fmap Right . f)
  {-# INLINE right' #-}

instance (Applicative m) => Traversing (EventSystem m) where
  traverse' :: (Traversable f) => EventSystem m a b -> EventSystem m (f a) (f b)
  traverse' (EventSystem m) = EventSystem (traverse m)

  wander :: (forall f. (Applicative f) => (a -> f b) -> s -> f t) -> EventSystem m a b -> EventSystem m s t
  wander f (EventSystem amb) = EventSystem (f amb)

instance (Applicative m, Distributive m) => Mapping (EventSystem m) where
  map' :: (Functor f) => EventSystem m a b -> EventSystem m (f a) (f b)
  map' (EventSystem f) = EventSystem (collect f)

  roam :: ((a -> b) -> s -> t) -> EventSystem m a b -> EventSystem m s t
  roam f = EventSystem #. genMap f .# dispatch
    where
      genMap :: (Distributive f) => ((a -> b) -> s -> t) -> (a -> f b) -> s -> f t
      genMap abst afb s = fmap (`abst` s) (distribute afb)

instance (Functor f) => Representable (EventSystem f) where
  type Rep (EventSystem f) = f

  tabulate :: (d -> Rep (EventSystem f) c) -> EventSystem f d c
  tabulate = EventSystem
  {-# INLINE tabulate #-}

instance (Functor f) => Sieve (EventSystem f) f where
  sieve :: EventSystem f a b -> a -> f b
  sieve = dispatch
  {-# INLINE sieve #-}

instance (Functor f) => Functor (EventSystem f a) where
  fmap :: (a1 -> b) -> EventSystem f a a1 -> EventSystem f a b
  fmap = rmap
  {-# INLINE fmap #-}

instance (Applicative f) => Applicative (EventSystem f a) where
  pure :: a1 -> EventSystem f a a1
  pure a = EventSystem $ \_ -> pure a

  (<*>) :: EventSystem f a (a1 -> b) -> EventSystem f a a1 -> EventSystem f a b
  EventSystem ff <*> EventSystem fx = EventSystem $ \a -> ff a <*> fx a

  (*>) :: EventSystem f a a1 -> EventSystem f a b -> EventSystem f a b
  EventSystem ff *> EventSystem fx = EventSystem $ \a -> ff a *> fx a

  (<*) :: EventSystem f a a1 -> EventSystem f a b -> EventSystem f a a1
  EventSystem ff <* EventSystem fx = EventSystem $ \a -> ff a <* fx a

instance (Alternative f) => Alternative (EventSystem f a) where
  empty :: EventSystem f a a1
  empty = EventSystem $ const empty

  (<|>) :: EventSystem f a a1 -> EventSystem f a a1 -> EventSystem f a a1
  EventSystem f <|> EventSystem g = EventSystem $ \a -> f a <|> g a

instance (Monad f) => Monad (EventSystem f a) where
  (>>=) :: EventSystem f a a1 -> (a1 -> EventSystem f a b) -> EventSystem f a b
  EventSystem m >>= f = EventSystem $ \e -> do
    a <- m e
    dispatch (f a) e

instance (MonadPlus f) => MonadPlus (EventSystem f a) where
  mzero :: EventSystem f a a1
  mzero = EventSystem $ const mzero

  mplus :: EventSystem f a a1 -> EventSystem f a a1 -> EventSystem f a a1
  EventSystem f `mplus` EventSystem g = EventSystem $ \a -> f a `mplus` g a

instance (Distributive f) => Distributive (EventSystem f a) where
  distribute :: (Functor f1) => f1 (EventSystem f a a1) -> EventSystem f a (f1 a1)
  distribute fs = EventSystem $ \a -> collect (($ a) .# dispatch) fs

instance (Monad f) => Category (EventSystem f) where
  id :: EventSystem f a a
  id = EventSystem return

  (.) :: EventSystem f b c -> EventSystem f a b -> EventSystem f a c
  EventSystem f . EventSystem g = EventSystem $ g >=> f

instance (Contravariant f) => Contravariant (EventSystem f a) where
  contramap :: (a' -> a1) -> EventSystem f a a1 -> EventSystem f a a'
  contramap f (EventSystem g) = EventSystem (contramap f . g)
  {-# INLINE contramap #-}

instance (Monad m) => Arrow (EventSystem m) where
  arr :: (b -> c) -> EventSystem m b c
  arr f = EventSystem (return . f)

  first :: EventSystem m b c -> EventSystem m (b, d) (c, d)
  first (EventSystem f) = EventSystem (\ ~(b, d) -> f b >>= \c -> return (c, d))

  second :: EventSystem m b c -> EventSystem m (d, b) (d, c)
  second (EventSystem f) = EventSystem (\ ~(d, b) -> f b >>= \c -> return (d, c))
