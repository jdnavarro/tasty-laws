{-# LANGUAGE CPP #-}
-- | This module is intended to be imported @qualified@:
--
-- > import qualified Test.Tasty.Laws.Monoid as Monoid
--
module Test.Tasty.Laws.Monoid
  ( test
  , testExhaustive
  , testMConcat
  , module Data.Monoid.Laws
  ) where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (Monoid)
#else
import Prelude hiding (mconcat)
#endif

import Test.DumbCheck (Series, uncurry3, zipA3)
import Data.Monoid.Laws
  ( leftIdentity
  , rightIdentity
  , associativity
  , mconcat
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.DumbCheck (testSeriesProperty)

-- | @tasty@ 'TestTree' for 'Monoid' laws. Sum of series for associativity law.
--
-- @
-- ('a' <> 'a') <> 'a' == 'a' <> ('a' <> 'a')
-- ('b' <> 'b') <> 'b' == 'b' <> ('b' <> 'b')
-- ('c' <> 'c') <> 'c' == 'c' <> ('c' <> 'c')
-- ...
-- @
test :: (Eq a, Show a, Monoid a) => Series a -> TestTree
test ms = testGroup "Monoid laws"
  [ testSeriesProperty "mempty <> x ≡ x" leftIdentity ms
  , testSeriesProperty "x <> mempty ≡ x" rightIdentity ms
  , testSeriesProperty "x <> (y <> z) ≡ (x <> y) <> z"
                       (uncurry3 associativity)
                       (zip3 ms ms ms)
  ]

-- | @tasty@ 'TestTree' for 'Monoid' laws. Product of series for associativity
--   law.
--
-- @
-- ('a' <> 'a') <> 'a' == 'a' <> ('a' <> 'a')
-- ('a' <> 'a') <> 'b' == 'a' <> ('a' <> 'b')
-- ('a' <> 'b') <> 'b' == 'a' <> ('b' <> 'b')
-- ...
-- @
testExhaustive :: (Eq a, Show a, Monoid a) => Series a -> TestTree
testExhaustive ms = testGroup "Monoid laws"
  [ testSeriesProperty "mempty <> x ≡ x" leftIdentity ms
  , testSeriesProperty "x <> mempty ≡ x" rightIdentity ms
  , testSeriesProperty "x <> (y <> z) ≡ (x <> y) <> z"
                       (uncurry3 associativity)
                       (zipA3 ms ms ms)
  ]

-- | Use this test when implementing the 'mconcat' method.
testMConcat :: (Eq a, Show a, Monoid a) => Series [a] -> TestTree
testMConcat ms = testSeriesProperty "mconcat ≡ foldr mappend mempty" mconcat ms
