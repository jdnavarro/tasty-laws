{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Tasty.SmallCheck.Laws.Applicative where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative)
#endif
import Data.Functor.Identity (Identity)
import Data.Proxy (Proxy(..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.SmallCheck (testProperty)
import Test.SmallCheck.Series (Series, Serial(series))

import qualified Test.SmallCheck.Laws.Applicative as Applicative
import Test.Tasty.SmallCheck.Laws.Functor

-- | @tasty@ 'TestTree' for 'Applicative' laws. You need to provide the type
--   wrapped in a `Proxy` and make sure 'a' is an instance of 'Serial'.
testApplicative
  :: forall f a .
     ( Applicative f
     , Show a, Eq a
     , Show (f a), Eq (f a), (Eq (f (f a)))
     , Show (f (a -> a))
     , Serial IO a
     , Serial IO (f a)
     , Serial IO (a -> a)
     , Serial IO (f (a -> a))
     , Serial Identity a, Serial Identity (f a)
     )
  => Proxy (f a) -> TestTree
testApplicative proxy = testGroup "Applicative"
  [ testFunctor proxy
  , testProperty "pure id <*> v ≡ v"
  $ Applicative.identity (series :: Series IO (f a))
  , testProperty "(.) <$> u <*> v <*> w ≡  u <*> (v <*> w)"
  $ Applicative.composition
      (series :: Series IO (f (a -> a)))
      (series :: Series IO (f a))
      (series :: Series IO (f (a -> a)))
  , testProperty "pure f <*> pure x ≡ pure (f x)" $ Applicative.homomorphism
      (Proxy :: Proxy f)
      (series :: Series IO a)
      (series :: Series IO (a -> a))
  , testProperty "u <*> pure y ≡ pure ($ y) <*> u" $ Applicative.interchange
      (series :: Series IO a)
      (series :: Series IO (f (a -> a)))
  ]
