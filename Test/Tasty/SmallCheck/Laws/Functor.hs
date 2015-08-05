{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Tasty.SmallCheck.Laws.Functor where

import Data.Proxy (Proxy)
import Data.Functor.Identity (Identity)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.SmallCheck (testProperty)
import Test.SmallCheck.Series (Serial(series), Series)

import qualified Test.SmallCheck.Laws.Functor as Functor

-- | @tasty@ 'TestTree' for 'Functor' laws. You need to provide the type
--   wrapped in a `Proxy` and make sure 'a' is an instance of 'Serial'.
testFunctor
  :: forall f a .
     ( Eq (f a), Eq (f (f a)), Functor f, Show a, Show (f a)
     , Serial IO (f a)
     , Serial IO (a -> a)
     , Serial Identity a, Serial Identity (f a)
     )
  => Proxy (f a) -> TestTree
testFunctor _ = testGroup "Functor laws"
  [ testProperty "fmap id ≡ id" $ Functor.identity (series :: Series IO (f a))
  , testProperty "fmap (f . g) ≡ fmap f . fmap g" $ Functor.composition
      (series :: Series IO (f a))
      (series :: Series IO (a -> a))
      (series :: Series IO (a -> a))
  ]
