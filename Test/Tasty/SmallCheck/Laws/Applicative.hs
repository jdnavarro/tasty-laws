{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Tasty.SmallCheck.Laws.Applicative where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative)
#endif
import Data.Proxy
import Data.Functor.Identity (Identity)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.SmallCheck (testProperty)
import Test.SmallCheck.Series (Series, Serial(series))

import Test.SmallCheck.Laws.Applicative

testApplicative
  :: forall f a .
     ( Applicative f
     , Show a, Eq a
     , Show (f a), Eq (f a)
     , Show (f (a -> a))
     , Serial IO a
     , Serial IO (f a)
     , Serial IO (a -> a)
     , Serial IO (f (a -> a))
     , Serial Identity a
     )
  => Proxy (f a) -> TestTree
testApplicative _ = testGroup "Applicative"
  [ testProperty "pure id <*> v ≡ v" $ identity (series :: Series IO (f a))
  , testProperty "pure (.) <*> u <*> v <*> w ≡  u <*> (v <*> w)" $ composition
      (series :: Series IO (f (a -> a)))
      (series :: Series IO (f a))
      (series :: Series IO (f (a -> a)))
  , testProperty "pure f <*> pure x ≡ pure (f x)" $ homomorphism
      (Proxy :: Proxy f)
      (series :: Series IO a)
      (series :: Series IO (a -> a))
  , testProperty "u <*> pure y ≡ pure ($ y) <*> u]" $ interchange
      (series :: Series IO a)
      (series :: Series IO (f (a -> a)))
  ]
