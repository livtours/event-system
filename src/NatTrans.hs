module NatTrans where

import "base" Data.Functor.Identity
import "base" Data.Kind (Type)
import "mmorph" Control.Monad.Morph hiding (embed)
import "transformers" Control.Monad.Trans.State

-- | This says that there is a natural transformation (another way to say this could be "a way of mapping a functor into another functor") between `m` and `n`
class Embeds m n where
  embed :: forall (a :: Type). m a -> n a

instance Embeds m m where
  embed :: m a -> m a
  embed = id

instance (Applicative m) => Embeds Identity m where
  embed :: Identity a -> m a
  embed = pure . runIdentity

-- if @m@ is a `Monad` and @t@ is a `MonadTrans`, then `Embeds n m` implies `Embeds n (t m)`
instance (Monad m, MonadTrans t, Embeds n m) => Embeds n (t m) where
  embed :: n a -> (t m) a
  embed = lift . embed

-- if @n@ is a `Monad` and @t@ is a `MFunctor`, then `Embeds n m` implies `Embeds (t n) (t m)`
instance {-# OVERLAPPING #-} (Monad n, MFunctor t, Embeds n m) => Embeds (t n) (t m) where
  embed :: t n a -> t m a
  embed = hoist embed

ioOperation :: IO ()
ioOperation = print "Marco"

statefulOperation :: State Int ()
statefulOperation = state (\i -> ((), i + 1))

combineTheTwo :: StateT Int IO ()
combineTheTwo = do
  embed ioOperation
  embed statefulOperation
