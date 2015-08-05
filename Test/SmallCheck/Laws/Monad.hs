{-# LANGUAGE FlexibleContexts #-}
{-|
Provided a 'Monad' is already an 'Applicative' there is no need to check the
first 2 monad laws. See
@<https://hackage.haskell.org/package/semigroupoids-5.0.0.2/docs/Data-Functor-Bind.html#t:Bind Bind>@
for which laws are exclusive for the '>>=' method.
-}
module Test.SmallCheck.Laws.Monad
  (
  -- * Monad laws
    associativity
  ) where

import Control.Monad ((>=>))
import Data.Functor.Identity (Identity)

import Test.SmallCheck (Property, over)
import Test.SmallCheck.Series (Serial, Series)
import Test.SmallCheck.Series.Utils (zipLogic3)

-- | Check the /associativity/ law hold for the given 'Monad' 'Series':
--
-- @
-- (m '>>=' f) '>>=' g ≡ m (f '>=>' g)
-- @
--
-- This is equivalent to:
--
-- @
-- (f '>=>' g) '>=>' h == f '>=>' (g '>=>' h)
-- @
--
-- Assuming 'join' is the default implementation of 'Monad'.

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
