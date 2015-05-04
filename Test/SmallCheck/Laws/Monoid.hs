module Test.SmallCheck.Laws.Monoid where

import Control.Applicative (liftA3)
import Data.Monoid ((<>))
import Test.SmallCheck (Property, over)
import Test.SmallCheck.Series ((>>-), Series, getDepth)

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
associativity s = over (liftA3 (,,) s s s) $ \(x,y,z) ->
    x <> (y <> z) == (x <> y) <> z

mconcatProp
  :: (Eq a, Monad m, Show a, Monoid a)
  => Series m a -> Property m
mconcatProp s = over (getDepth >>- \d -> sequenceA (replicate d s)) $ \l ->
    mconcat l == foldr mappend mempty l
