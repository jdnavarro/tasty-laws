{-# LANGUAGE CPP #-}
module Test.SmallCheck.Laws.Monoid where

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

leftIdentity
  :: (Eq a, Monad m, Show a, Monoid a)
  => Series m a -> Property m
leftIdentity s = over s $ \x -> mempty <> x == x

rightIdentity
  :: (Eq a, Monad m, Show a, Monoid a)
  => Series m a -> Property m
rightIdentity s = over s $ \x -> x <> mempty == x

associativity
  :: (Eq a, Monad m, Show a, Monoid a)
  => Series m a -> Property m
associativity s =
    over s $ \x ->
        over s $ \y ->
            over s $ \z ->
                x <> (y <> z) == (x <> y) <> z

mconcat
  :: (Eq a, Monad m, Show a, Monoid a)
  => Series m a -> Property m
mconcat s = over (sequenceA $ replicate 3 s) $ \l ->
    Monoid.mconcat l == foldr mappend mempty l
