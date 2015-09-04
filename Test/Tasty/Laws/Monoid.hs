{-# LANGUAGE CPP #-}
-- | This module is intended to be imported @qualified@:
--
-- > import qualified Test.Tasty.Laws as Monoid
--
module Test.Tasty.Laws.Monoid where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (Monoid)
#endif

import Test.SmallCheck.Series (Series)
import qualified Test.SmallCheck.Laws.Monoid as Monoid
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.SmallCheck (testProperty)

-- | @tasty@ 'TestTree' for 'Monoid' laws. Sum of series for associativity law.
--
-- @
-- ('a' <> 'a') <> 'a' == 'a' <> ('a' <> 'a')
-- ('b' <> 'b') <> 'b' == 'b' <> ('b' <> 'b')
-- ('c' <> 'c') <> 'c' == 'c' <> ('c' <> 'c')
-- ...
-- @
test :: (Eq a, Show a, Monoid a) => Series IO a -> TestTree
test ms = testGroup "Monoid laws"
  [ testProperty "mempty <> x ≡ x" $ Monoid.leftIdentity ms
  , testProperty "x <> mempty ≡ x" $ Monoid.rightIdentity ms
  , testProperty "x <> (y <> z) ≡ (x <> y) <> z"
        $ Monoid.associativitySum ms ms ms
  ]

-- | @tasty@ 'TestTree' for 'Monoid' laws. Product of series for associativity
--   law. Be aware of combinatorial explosion.
--
-- @
-- ('a' <> 'a') <> 'a' == 'a' <> ('a' <> 'a')
-- ('a' <> 'a') <> 'b' == 'a' <> ('a' <> 'b')
-- ('a' <> 'b') <> 'b' == 'a' <> ('b' <> 'b')
-- ...
-- @
testExhaustive :: (Eq a, Show a, Monoid a) => Series IO a -> TestTree
testExhaustive ms = testGroup "Monoid laws"
  [ testProperty "mempty <> x ≡ x" $ Monoid.leftIdentity ms
  , testProperty "x <> mempty ≡ x" $ Monoid.rightIdentity ms
  , testProperty "x <> (y <> z) ≡ (x <> y) <> z"
        $ Monoid.associativity ms ms ms
  ]

-- | Use this test when implementing the 'mconcat' method.
testMConcat :: (Eq a, Show a, Monoid a) => Series IO a -> TestTree
testMConcat ms = testProperty "mconcat ≡ foldr mappend mempty" $ Monoid.mconcat ms
