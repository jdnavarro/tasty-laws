{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | This module is intended to be imported @qualified@, for example:
--
-- > import qualified Test.Tasty.Laws.Monad as Monad
--
module Test.Tasty.Laws.Monad
  ( testUnit
  , test
  , testExhaustive
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative)
#endif
import Control.Monad.Laws (associativity)
import Test.DumbCheck (Series, Serial(series), uncurry3, zipA3)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.DumbCheck (testSeriesProperty)
import qualified Test.Tasty.Laws.Applicative as Applicative

-- | @tasty@ 'TestTree' for 'Monad' laws. The type signature forces the
--   parameter to be '()' which, unless you are dealing with non-total
--   functions, should be enough to test any 'Monad's.
testUnit
  :: ( Applicative m, Monad m
     , Eq (m ())
     , Show (m ()), Show (m (() -> ()))
     , Serial (m ()), Serial (m (() -> ()))
     )
    => Series (m ()) -> TestTree
testUnit = testExhaustive

-- | @tasty@ 'TestTree' for 'Monad' laws. Monomorphic sum 'Series'.
test
  :: forall m a .
     ( Applicative m, Monad m
     , Eq a, Eq (m a)
     , Show a, Show (m a), Show (m (a -> a))
     , Serial a, Serial (m a), Serial (m (a -> a))
     )
  => Series (m a) -> TestTree
test ms = testGroup "Monad laws"
  [ Applicative.test ms
  , testSeriesProperty
      "(m >>= f) >>= g ≡ m (f >=> g)"
      (uncurry3 associativity)
      $ zip3 ms
             (series :: Series (a -> m a))
             (series :: Series (a -> m a))
  ]

-- | @tasty@ 'TestTree' for 'Monad' laws. Monomorphic product 'Series'.
testExhaustive
  :: forall m a .
     ( Applicative m, Monad m
     , Eq a, Eq (m a)
     , Show a, Show (m a), Show (m (a -> a))
     , Serial a, Serial (m a), Serial (m (a -> a))
     )
  => Series (m a) -> TestTree
testExhaustive ms = testGroup "Monad laws"
  [ Applicative.testExhaustive ms
  , testSeriesProperty
      "(m >>= f) >>= g ≡ m (f >=> g)"
      (uncurry3 associativity)
      $ zipA3 ms
              (series :: Series (a -> m a))
              (series :: Series (a -> m a))
  ]
