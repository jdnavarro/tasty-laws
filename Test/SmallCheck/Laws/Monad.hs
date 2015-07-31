{-# LANGUAGE FlexibleContexts #-}
module Test.SmallCheck.Laws.Monad where

import Control.Monad ((>=>))
import Data.Functor.Identity (Identity)

import Test.SmallCheck (Property, over)
import Test.SmallCheck.Series (Serial, Series)
import Test.SmallCheck.Series.Utils (zipLogic3)

-- This is equivalent to `(f >=> g) >=> h == f >=> (g >=> h)` which requires
-- the constraint `Eq (a -> f b)`. `Eq (f a)` is much easier to deal with.
associativity
  :: ( Monad m, Monad f
     , Show a, Show b, Show c, Show (f a), Show (f b), Show (f c)
     , Eq (f a), Eq (f c)
     , Serial Identity a, Serial Identity b, Serial Identity c
     )
  => Series m (f a)
  -> Series m (a -> f b)
  -> Series m (b -> f c)
  -> Property m
associativity ms fs gs = over (zipLogic3 ms fs gs) $ \(m,f,g) ->
    (m >>= f >>= g) == (m >>= (f >=> g))
