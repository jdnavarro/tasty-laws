{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | This module is intended to be imported @qualified@, for example:
--
-- > import qualified Test.Tasty.Laws.Applicative as Applicative
--
module Test.Tasty.Laws.Applicative
  ( test
  , testMono
  , testMonoExhaustive
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative)
#endif
import Data.Functor.Identity (Identity)
import Data.Proxy (Proxy(..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.SmallCheck (testProperty)
import Test.SmallCheck.Series (Series, Serial(series))

import Test.SmallCheck.Laws.Applicative
  ( identity
  , composition
  , compositionSum
  , homomorphism
  , homomorphismSum
  , interchange
  , interchangeSum
  )
import qualified Test.Tasty.Laws.Functor as Functor

test
  :: ( Applicative f
     , Eq (f ()), Eq (f (f ())), Show (f ()), Show (f (() -> ()))
     , Serial Identity (f ())
     , Serial IO (f ()), Serial IO (f (() -> ()))
     )
  => Series IO (f ()) -> TestTree
test = testMonoExhaustive

-- | @tasty@ 'TestTree' for 'Applicative' laws. You need to provide the type
--   wrapped in a `Proxy` and make sure 'a' is an instance of 'Serial'.
testMono
  :: forall f a .
     ( Applicative f
     , Eq a, Eq (f a), (Eq (f (f a)))
     , Show a, Show (f a), Show (f (a -> a))
     , Serial Identity a, Serial Identity (f a)
     , Serial IO a, Serial IO (f a), Serial IO (a -> a), Serial IO (f (a -> a))
     )
  => Series IO  (f a) -> TestTree
testMono fs = testGroup "Applicative"
  [ Functor.testMono fs
  , testProperty "pure id <*> v ≡ v" $ identity fs
  , testProperty "(.) <$> u <*> v <*> w ≡  u <*> (v <*> w)" $ compositionSum
      (series :: Series IO (f (a -> a)))
      (series :: Series IO (f a))
      (series :: Series IO (f (a -> a)))
  , testProperty "pure f <*> pure x ≡ pure (f x)" $ homomorphismSum
      (Proxy  :: Proxy f)
      (series :: Series IO a)
      (series :: Series IO (a -> a))
  , testProperty "u <*> pure y ≡ pure ($ y) <*> u" $ interchangeSum
      (series :: Series IO a)
      (series :: Series IO (f (a -> a)))
  ]

-- | @tasty@ 'TestTree' for 'Applicative' laws. You need to provide the type
--   wrapped in a `Proxy` and make sure 'a' is an instance of 'Serial'.
testMonoExhaustive
  :: forall f a .
     ( Applicative f
     , Eq a, Eq (f a), (Eq (f (f a)))
     , Show a, Show (f a), Show (f (a -> a))
     , Serial Identity a, Serial Identity (f a)
     , Serial IO a, Serial IO (f a), Serial IO (a -> a), Serial IO (f (a -> a))
     )
  => Series IO  (f a) -> TestTree
testMonoExhaustive fs = testGroup "Applicative"
  [ Functor.testMonoExhaustive fs
  , testProperty "pure id <*> v ≡ v" $ identity fs
  , testProperty "(.) <$> u <*> v <*> w ≡  u <*> (v <*> w)" $ composition
      (series :: Series IO (f (a -> a)))
      (series :: Series IO (f a))
      (series :: Series IO (f (a -> a)))
  , testProperty "pure f <*> pure x ≡ pure (f x)" $ homomorphism
      (Proxy  :: Proxy f)
      (series :: Series IO a)
      (series :: Series IO (a -> a))
  , testProperty "u <*> pure y ≡ pure ($ y) <*> u" $ interchange
      (series :: Series IO a)
      (series :: Series IO (f (a -> a)))
  ]
