{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
module Test.Tasty.SmallCheck.Laws.Monoid where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (Monoid)
#endif
import Test.SmallCheck.Series (Serial, Series)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.SmallCheck (testProperty)

import Test.SmallCheck.Laws.Monoid

testMonoidLaws :: (Show a, Eq a, Monoid a, Serial IO a) => Series IO a -> TestTree
testMonoidLaws s = testGroup "Monoid laws"
  [ testProperty "mempty <> x = x" $ leftIdentity s
  , testProperty "x <> mempty = x" $ rightIdentity s
  , testProperty "x <> (y <> z) = (x <> y) <> z" $ associativity s
  , testProperty "mconcat = foldr mappend mempty" $ mconcatProp s
  ]
