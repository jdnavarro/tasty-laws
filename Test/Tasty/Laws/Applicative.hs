{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | This module is intended to be imported @qualified@, for example:
--
-- > import qualified Test.Tasty.Laws.Applicative as Applicative
--
module Test.Tasty.Laws.Applicative
  ( testUnit
  , test
  , testExhaustive
  , module Control.Applicative.Laws
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative)
#endif
import Data.Proxy (Proxy(..))
import Test.Tasty (TestTree, testGroup)
import Test.DumbCheck (uncurry3, zipA2, zipA3)
import Test.Tasty.DumbCheck (Serial(series), Series, testSeriesProperty)
import Control.Applicative.Laws
  ( identity
  , composition
  , homomorphism
  , interchange
  )

import qualified Test.Tasty.Laws.Functor as Functor

-- | @tasty@ 'TestTree' for 'Applicative' laws. The type signature forces the
--   parameter to be '()' which, unless you are dealing with non-total
--   functions, should be enough to test any 'Applicative's.
-- test
--   :: ( Applicative f
--      , Eq (f ()), Eq (f (f ())), Show (f ()), Show (f (() -> ()))
--      , Serial Identity (f ())
--      , Serial IO (f ()), Serial IO (f (() -> ()))
--      )
testUnit
  :: ( Applicative f
     , Eq (f ())
     , Show (f ()), Show (f (() -> ()))
     , Serial (f ()), Serial (f (() -> ()))
     )
  => Series (f ()) -> TestTree
testUnit = testExhaustive

-- | @tasty@ 'TestTree' for 'Applicative' laws. Monomorphic sum 'Series'.
test
  :: forall f a .
     ( Applicative f
     , Eq a, Eq (f a)
     , Show a, Show (f a), Show (f (a -> a))
     , Serial a, Serial (f a), Serial (f (a -> a))
     )
  => Series (f a) -> TestTree
test fs = testGroup "Applicative"
  [ Functor.test fs
  , testSeriesProperty "pure id <*> v ≡ v" identity fs
  , testSeriesProperty
      "(.) <$> u <*> v <*> w ≡  u <*> (v <*> w)"
      (uncurry3 composition)
      $ zip3 (series :: Series (f (a -> a)))
             (series :: Series (f a))
             (series :: Series (f (a -> a)))
  , testSeriesProperty
      "pure f <*> pure x ≡ pure (f x)"
      (uncurry3 homomorphism)
      $ zip3 (repeat (Proxy  :: Proxy f))
             (series :: Series a)
             (series :: Series (a -> a))
  , testSeriesProperty
      "u <*> pure y ≡ pure ($ y) <*> u"
      (uncurry interchange)
      $ zip (series :: Series a)
            (series :: Series (f (a -> a)))
  ]

testExhaustive
  :: forall f a .
     ( Applicative f
     , Eq a, Eq (f a)
     , Show a, Show (f a), Show (f (a -> a))
     , Serial a, Serial (f a), Serial (f (a -> a))
     )
  => Series (f a) -> TestTree
testExhaustive fs = testGroup "Applicative"
  [ Functor.testExhaustive fs
  , testSeriesProperty "pure id <*> v ≡ v" identity fs
  , testSeriesProperty
      "(.) <$> u <*> v <*> w ≡  u <*> (v <*> w)"
      (uncurry3 composition)
      $ zipA3 (series :: Series (f (a -> a)))
              (series :: Series (f a))
              (series :: Series (f (a -> a)))
  , testSeriesProperty
      "pure f <*> pure x ≡ pure (f x)" (uncurry3 homomorphism)
      $ zipA3 (repeat (Proxy  :: Proxy f))
              (series :: Series a)
              (series :: Series (a -> a))
  , testSeriesProperty
      "u <*> pure y ≡ pure ($ y) <*> u"
      (uncurry interchange)
      $ zipA2 (series :: Series a)
              (series :: Series (f (a -> a)))
  ]
