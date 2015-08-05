{-# LANGUAGE CPP #-}
module Test.SmallCheck.Laws.Monoid
  (
  -- * Monoid laws
    leftIdentity
  , rightIdentity
  , associativity
  , mconcat
  ) where

#if MIN_VERSION_base(4,8,0)
import Prelude hiding (mconcat)
#else
import Data.Monoid (Monoid, mappend, mempty)
import Data.Traversable (sequenceA)
#endif
import Data.Monoid ((<>))
import qualified Data.Monoid as Monoid (mconcat)
import Test.SmallCheck (Property, over)
import Test.SmallCheck.Series (Series)
import Test.SmallCheck.Series.Utils (zipLogic3)

-- | Check the /left identity/ law hold for the given 'Monoid' 'Series':
--
-- @
-- 'mempty' '<>' x ≡ x
-- @
leftIdentity
  :: (Eq a, Monad m, Show a, Monoid a)
  => Series m a -> Property m
leftIdentity s = over s $ \x -> mempty <> x == x

-- | Check the /right identity/ law hold for the given 'Monoid' 'Series':
--
-- @
-- x '<>' 'mempty' ≡ x
-- @
rightIdentity
  :: (Eq a, Monad m, Show a, Monoid a)
  => Series m a -> Property m
rightIdentity s = over s $ \x -> x <> mempty == x

-- | Check the /associativity/ law hold for the given 'Monoid' 'Series':
--
-- @
-- x '<>' (y '<>' z) ≡ (x '<>' y) '<>' z
-- @
associativity
  :: (Eq a, Monad m, Show a, Monoid a)
  => Series m a -> Series m a -> Series m a -> Property m
associativity xs ys zs =
    over (zipLogic3 xs ys zs) $ \(x,y,z) ->
        x <> (y <> z) == (x <> y) <> z

-- | Check the /mconcat/ law hold for the given 'Monoid' 'Series':
--
-- @
-- 'mconcat' ≡ 'foldr' 'mappend' 'mempty'
-- @
mconcat
  :: (Eq a, Monad m, Show a, Monoid a)
  => Series m a -> Property m
mconcat s = over (sequenceA $ replicate 3 s) $ \l ->
    Monoid.mconcat l == foldr mappend mempty l
