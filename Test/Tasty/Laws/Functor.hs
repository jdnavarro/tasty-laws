{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | This module is intended to be imported @qualified@, for example:
--
-- > import qualified Test.Tasty.Laws.Functor as Functor
--
module Test.Tasty.Laws.Functor
  ( testUnit
  , test
  , testExhaustive
  , testSeries
  , testSeriesExhaustive
  , module Data.Functor.Laws
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Test.DumbCheck (Serial(series), Series, uncurry3, zipA3)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.DumbCheck (testSeriesProperty)
import Data.Functor.Laws (identity, composition)
import Text.Show.Functions ()

-- | @tasty@ 'TestTree' for 'Functor' laws. The type parameter for the
--  'Functor' is '()' which unless you are dealing with partial functions,
--  should be enough to test any 'Functor'.
testUnit :: (Functor f, Eq (f ()), Show (f ())) => Series (f ()) -> TestTree
testUnit ss = testSeries1 $ zipA3 ss [const ()] [const ()]

-- | @tasty@ 'TestTree' for 'Functor' laws. Monomorphic sum 'Series' for @f@
--   and @g@ in the compose law.
--
-- @
-- fmap (\a -> a) . (\a -> a) == fmap (\a -> a) . fmap (\a -> a)
-- fmap (\b -> b) . (\b -> b) == fmap (\b -> b) . fmap (\b -> b)
-- ...
-- @
test
  :: forall f a . (Eq (f a), Functor f, Show (f a), Serial a)
  => Series (f a) -> TestTree
test ss = testSeries1 $ zip3 ss (series :: Series (a -> a))
                                (series :: Series (a -> a))

-- | @tasty@ 'TestTree' for 'Functor' laws. Monomorphic product 'Series' for
--   @f@ and @g@ in the compose law.
--
-- @
-- fmap (\a -> a) . (\a -> a) == fmap (\a -> a) . fmap (\a -> a)
-- fmap (\a -> a) . (\a -> b) == fmap (\a -> a) . fmap (\a -> b)
-- fmap (\a -> a) . (\b -> b) == fmap (\a -> a) . fmap (\b -> b)
-- ...
-- @
testExhaustive
  :: forall f a . (Eq (f a), Functor f, Show (f a), Serial a)
  => Series (f a) -> TestTree
testExhaustive ss = testSeries1 $ zipA3 ss (series :: Series (a -> a))
                                           (series :: Series (a -> a))

-- | @tasty@ 'TestTree' for 'Functor' laws. Polymorphic sum 'Series' for
--   @f@ and @g@ in the compose law.
--
-- @
-- fmap (\a0 -> b0) . (\b0 -> c0) == fmap (\a0 -> b0) . fmap (\b0 -> c0)
-- fmap (\a1 -> b1) . (\b1 -> c1) == fmap (\a1 -> a1) . fmap (\b1 -> c1)
-- fmap (\a2 -> b2) . (\b2 -> c2) == fmap (\a2 -> a2) . fmap (\b2 -> c2)
-- ...
-- @
-- testPoly
--   :: forall f a b c .
--      ( Functor f
--      , Eq (f a), Show a, Show (f a) , Serial a
--      , Eq (f b), Show b, Show (f b) , Serial b
--      , Eq (f c), Show c, Show (f c) , Serial c
--      , Serial (a -> b), Serial (b -> c)
--      )
--   => Proxy b -> Proxy c -> Series (f a) -> TestTree
-- testPoly _ _ = testWithComp $ \fs ->
--     composition fs (series :: Series (b -> c))
--                    (series :: Series (a -> b))

-- | @tasty@ 'TestTree' for 'Functor' laws with explict sum 'Series' for
--   @f@ and @g@ in the compose law.
--
-- @
-- fmap (\a0 -> b0) . (\b0 -> c0) == fmap (\a0 -> b0) . fmap (\b0 -> c0)
-- fmap (\a1 -> b1) . (\b1 -> c1) == fmap (\a1 -> a1) . fmap (\b1 -> c1)
-- fmap (\a1 -> b1) . (\b1 -> c1) == fmap (\a1 -> a1) . fmap (\b1 -> c1)
-- ...
-- @
testSeries
  :: ( Eq (f c), Eq (f b), Functor f, Show (f c))
  => Series (f c) -> Series (a -> b) -> Series (c -> a) -> TestTree
testSeries xs fs gs = testSeries1 (zip3 xs fs gs)

-- | @tasty@ 'TestTree' for 'Functor' laws with explicit product 'Series'
--   for @f@ and @g@ in the compose law.
--
-- @
-- fmap (\a0 -> b0) . (\b0 -> c0) == fmap (\a0 -> b0) . fmap (\b0 -> c0)
-- fmap (\a0 -> b0) . (\b0 -> c1) == fmap (\a0 -> a0) . fmap (\b0 -> c1)
-- fmap (\a0 -> b0) . (\b0 -> c0) == fmap (\a0 -> a0) . fmap (\b1 -> c1)
-- ...
-- @
testSeriesExhaustive
  :: ( Eq (f c), Eq (f b), Functor f, Show (f c))
  => Series (f c) -> Series (a -> b) -> Series (c -> a) -> TestTree
testSeriesExhaustive xs fs gs = testSeries1 (zipA3 xs fs gs)

testSeries1
  :: (Eq (f c), Eq (f b), Functor f, Show (f c))
  => Series (f c, a -> b, c -> a) -> TestTree
testSeries1 ss = testGroup "Functor laws"
  [ testSeriesProperty "fmap id ≡ id" identity ((\(x,_,_) -> x) <$> ss)
  , testSeriesProperty "fmap (f . g) ≡ fmap f . fmap g" (uncurry3 composition) ss
  ]
