{-# LANGUAGE FlexibleContexts #-}
module Test.SmallCheck.Laws.Functor
  (
  -- * Functor laws
    identity
  , composition
  ) where

import Data.Functor.Identity (Identity)
import Test.SmallCheck (Property, over)
import Test.SmallCheck.Series (Serial, Series)
import Test.SmallCheck.Series.Utils (zipLogic3)

-- | Check the /identity/ law hold for the given 'Functor' 'Series':
--
-- @
-- 'fmap' 'id' ≡ 'id'
-- @
identity
  :: (Eq (f a), Monad m, Show (f a), Functor f)
  => Series m (f a) -> Property m
identity s = over s $ \x -> fmap id x == x

-- | Check the /composition/ law hold for the given 'Functor' 'Series':
--
-- @
-- 'fmap' (f . g) ≡ 'fmap' f . 'fmap' g
-- @
composition
  :: ( Monad m, Functor f, Show a, Show b, Show c
     , Show (f a), Eq (f c)
     , Serial Identity a, Serial Identity b
     )
  => Series m (f a) -> Series m (b -> c) -> Series m (a -> b) -> Property m
composition xs fs gs = over (zipLogic3 xs fs gs) $ \(x,f,g) ->
    fmap (f . g) x == (fmap f . fmap g) x
