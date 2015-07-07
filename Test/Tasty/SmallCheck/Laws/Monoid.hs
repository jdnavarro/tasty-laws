{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
module Test.Tasty.SmallCheck.Laws.Monoid where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (Monoid)
#endif
import Test.SmallCheck.Series (Series)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.SmallCheck (testProperty)

import Test.SmallCheck.Laws.Monoid

testMonoid :: (Show a, Eq a, Monoid a) => Series IO a -> TestTree
testMonoid s = testGroup "Monoid laws" $ map ($ s)
  [ testProperty "mempty <> x = x" . leftIdentity
  , testProperty "x <> mempty = x" . rightIdentity
  , testProperty "x <> (y <> z) = (x <> y) <> z" . associativity
  , testProperty "mconcat = foldr mappend mempty" . mconcatProp
  ]
