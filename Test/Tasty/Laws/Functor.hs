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
  ) where

import Data.Proxy
import Data.Functor.Identity (Identity)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.SmallCheck (testProperty, Testable)
import Test.SmallCheck.Laws.Functor (identity, composition, compositionSum)
import Test.SmallCheck.Series (Serial(series), Series)

-- | @tasty@ 'TestTree' for 'Functor' laws. You need to provide the type
--   wrapped in a `Proxy` and make sure 'a' is an instance of 'Serial'.
test :: (Functor f, Eq (f ()), Show (f ())) => Series IO (f ()) -> TestTree
test = testMono

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
    composition fs (series :: Series IO (b -> c))
                   (series :: Series IO (a -> b))

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

testWithComp
  :: (Eq (f a), Functor f, Show (f a), Testable IO r)
  => (Series IO (f a) -> r) -> Series IO (f a) -> TestTree
testWithComp comp fs = testGroup "Functor laws"
  [ testProperty "fmap id ≡ id" $ identity fs
  , testProperty "fmap (f . g) ≡ fmap f . fmap g"
    $ comp fs
  ]
