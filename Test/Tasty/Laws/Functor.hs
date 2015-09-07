{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | This module is intended to be imported @qualified@, for example:
--
-- > import qualified Test.Tasty.Laws.Functor as Functor
--
module Test.Tasty.Laws.Functor
  ( test
  , testMono
  , testMonoExhaustive
  , testPoly
  , testPolyExhaustive
  , module Test.SmallCheck.Laws.Functor
  ) where

import Data.Proxy
import Data.Functor.Identity (Identity)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.SmallCheck (testProperty, Testable)
import Test.SmallCheck.Laws.Functor (identity, composition, compositionSum)
import Test.SmallCheck.Series (Serial(series), Series)

-- | @tasty@ 'TestTree' for 'Functor' laws. The type signature forces the
--   parameter to be '()' which, unless you are dealing non-total functions,
--   should be enough to test any 'Functor's.
test :: (Functor f, Eq (f ()), Show (f ())) => Series IO (f ()) -> TestTree
test = testMonoExhaustive

-- | @tasty@ 'TestTree' for 'Functor' laws. Monomorphic sum 'Series' for @f@
--   and @g@ in the compose law.
--
-- @
-- fmap (\a -> a) . (\a -> a) == fmap (\a -> a) . fmap (\a -> a)
-- fmap (\b -> b) . (\b -> b) == fmap (\b -> b) . fmap (\b -> b)
-- ...
-- @
testMono
  :: forall f a .
     ( Eq (f a), Functor f, Show a, Show (f a)
     , Serial Identity a
     , Serial IO (a -> a)
     )
  => Series IO (f a) -> TestTree
testMono = testWithComp $ \fs ->
    compositionSum fs (series :: Series IO (a -> a))
                      (series :: Series IO (a -> a))

-- | @tasty@ 'TestTree' for 'Functor' laws. Monomorphic product 'Series' for
--   @f@ and @g@ in the compose law.
--
-- @
-- fmap (\a -> a) . (\a -> a) == fmap (\a -> a) . fmap (\a -> a)
-- fmap (\a -> a) . (\a -> b) == fmap (\a -> a) . fmap (\a -> b)
-- fmap (\a -> a) . (\b -> b) == fmap (\a -> a) . fmap (\b -> b)
-- ...
-- @
testMonoExhaustive
  :: forall f a .
     ( Eq (f a), Functor f, Show a, Show (f a)
     , Serial Identity a
     , Serial IO (a -> a)
     )
  => Series IO (f a) -> TestTree
testMonoExhaustive = testWithComp $ \fs ->
    composition fs (series :: Series IO (a -> a))
                   (series :: Series IO (a -> a))

-- | @tasty@ 'TestTree' for 'Functor' laws. Polymorphic sum 'Series' for
--   @f@ and @g@ in the compose law.
--
-- @
-- fmap (\a0 -> b0) . (\b0 -> c0) == fmap (\a0 -> b0) . fmap (\b0 -> c0)
-- fmap (\a1 -> b1) . (\b1 -> c1) == fmap (\a1 -> a1) . fmap (\b1 -> c1)
-- fmap (\a2 -> b2) . (\b2 -> c2) == fmap (\a2 -> a2) . fmap (\b2 -> c2)
-- ...
-- @
testPoly
  :: forall f a b c .
     ( Functor f
     , Eq (f a), Show a, Show (f a) , Serial Identity a
     , Eq (f b), Show b, Show (f b) , Serial Identity b
     , Eq (f c), Show c, Show (f c) , Serial Identity c
     , Serial IO (a -> b), Serial IO (b -> c)
     )
  => Proxy b -> Proxy c -> Series IO (f a) -> TestTree
testPoly _ _ = testWithComp $ \fs ->
    compositionSum fs (series :: Series IO (b -> c))
                      (series :: Series IO (a -> b))

-- | @tasty@ 'TestTree' for 'Functor' laws. Polymorphic product 'Series' for
--   @f@ and @g@ in the compose law.
--
-- @
-- fmap (\a0 -> b0) . (\b0 -> c0) == fmap (\a0 -> b0) . fmap (\b0 -> c0)
-- fmap (\a0 -> b0) . (\b0 -> c1) == fmap (\a0 -> a0) . fmap (\b0 -> c1)
-- fmap (\a0 -> b0) . (\b0 -> c0) == fmap (\a0 -> a0) . fmap (\b1 -> c1)
-- ...
-- @
testPolyExhaustive
  :: forall f a b c .
     ( Functor f
     , Eq (f a), Show a, Show (f a) , Serial Identity a
     , Eq (f b), Show b, Show (f b) , Serial Identity b
     , Eq (f c), Show c, Show (f c) , Serial Identity c
     , Serial IO (a -> b), Serial IO (b -> c)
     )
  => Proxy b -> Proxy c -> Series IO (f a) -> TestTree
testPolyExhaustive _ _ = testWithComp $ \fs ->
    composition fs (series :: Series IO (b -> c))
                   (series :: Series IO (a -> b))

-- * Internal

testWithComp
  :: (Eq (f a), Functor f, Show (f a), Testable IO r)
  => (Series IO (f a) -> r) -> Series IO (f a) -> TestTree
testWithComp comp fs = testGroup "Functor laws"
  [ testProperty "fmap id ≡ id" $ identity fs
  , testProperty "fmap (f . g) ≡ fmap f . fmap g"
    $ comp fs
  ]
