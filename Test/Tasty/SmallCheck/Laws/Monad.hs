{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Tasty.SmallCheck.Laws.Monad where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative)
#endif
import Data.Functor.Identity (Identity)
import Data.Proxy (Proxy(..))
import Test.SmallCheck.Series (Series, Serial(series))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.SmallCheck (testProperty)

import Test.Tasty.SmallCheck.Laws.Applicative
import qualified Test.SmallCheck.Laws.Monad as Monad

-- | @tasty@ 'TestTree' for 'Monad' laws. You need to provide the type
--   wrapped in a `Proxy` and make sure 'a' is an instance of 'Serial'.
testMonad
  :: forall f a .
     ( Applicative f, Monad f
     , Show a, Show (f a), Show (f (a -> a))
     , Eq a, Eq (f a), Eq (f (f a))
     , Serial IO a, Serial IO (a -> a)
     , Serial IO (f a) ,Serial IO (f (a -> a)), Serial IO (a -> f a)
     , Serial Identity a, Serial Identity (f a)
     )
  => Proxy (f a) -> TestTree
testMonad proxy = testGroup "Monad laws"
  [ testApplicative proxy
  , testProperty "(m >>= f) >>= g ≡ m (f >=> g)"
  $ Monad.associativity (series :: Series IO (f a))
                        (series :: Series IO (a -> f a))
                        (series :: Series IO (a -> f a))
  ]
