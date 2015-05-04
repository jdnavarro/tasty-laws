{-# LANGUAGE FlexibleContexts #-}
module Test.Tasty.SmallCheck.Laws.Functor where

import Data.Functor.Identity (Identity)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.SmallCheck (testProperty)
import Test.SmallCheck.Series (Serial, CoSerial, Series)

import Test.SmallCheck.Laws.Functor

testFunctorLaws
  :: ( Eq (f a), Eq (f (f a)), Functor f, Show a, Show (f a)
     , Serial Identity a, Serial Identity (f a)
     , CoSerial IO a, CoSerial IO (f a)
     )
  => Series IO (f a) -> TestTree
testFunctorLaws s = testGroup "Functor laws"
  [ testProperty "fmap id == id" $ fmapIdentity s
  , testProperty "fmap (f . g) == fmap f . fmap g" $ fmapCompose s
  ]
