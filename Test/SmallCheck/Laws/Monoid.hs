{-# LANGUAGE CPP #-}
module Test.SmallCheck.Laws.Monoid where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (Monoid, mappend, mempty, mconcat)
import Data.Traversable (sequenceA)
#endif
import Data.Monoid ((<>))
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

mconcatProp
  :: (Eq a, Monad m, Show a, Monoid a)
  => Series m a -> Property m
mconcatProp s = over (sequenceA $ replicate 3 s) $ \l ->
    mconcat l == foldr mappend mempty l
