{-# LANGUAGE RebindableSyntax #-}

module RebindedDo where

import NatTrans

import "transformers" Control.Monad.Trans.State
import "base" Prelude hiding ((>>), (>>=))
import "base" Prelude qualified ((>>=))

(>>=) :: (Monad m, Embeds n1 m, Embeds n2 m) => n1 a -> (a -> n2 b) -> m b
(>>=) n1a an2b = embed n1a Prelude.>>= (embed . an2b)

(>>) :: (Monad m, Embeds n1 m, Embeds n2 m) => n1 a -> n2 b -> m b
(>>) n1a n2b = n1a >>= const n2b

combineTheTwoRebinded :: StateT Int IO ()
combineTheTwoRebinded = do
  ioOperation
  statefulOperation
