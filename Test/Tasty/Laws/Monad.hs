{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | This module is intended to be imported @qualified@, for example:
--
-- > import qualified Test.Tasty.Laws.Monad as Monad
--
module Test.Tasty.Laws.Monad
  ( test
  , testMono
  , testMonoExhaustive
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative)
#endif
import Data.Functor.Identity (Identity)
import Test.SmallCheck.Series (Series, Serial(series))
import Test.SmallCheck.Laws.Monad (associativity, associativitySum)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.SmallCheck (testProperty)
import qualified Test.Tasty.Laws.Applicative as Applicative

-- | @tasty@ 'TestTree' for 'Monad' laws. The type signature forces the
--   parameter to be '()' which, unless you are dealing with non-total
--   functions, should be enough to test any 'Monad's.
test
  :: ( Applicative m, Monad m
     , Eq (m ()), Eq (m (m ()))
     , Show (m ()), Show (m (() -> ()))
     , Serial Identity (m ())
     , Serial IO (m ()), Serial IO (m (() -> ()))
     )
  => Series IO (m ()) -> TestTree
test = testMonoExhaustive

-- | @tasty@ 'TestTree' for 'Monad' laws. Monomorphic sum 'Series'.
testMono
  :: forall m a .
     ( Applicative m, Monad m
     , Eq a, Eq (m a), Eq (m (m a))
     , Show a, Show (m a), Show (m (a -> a))
     , Serial Identity a, Serial Identity (m a)
     , Serial IO a, Serial IO (a -> a)
     , Serial IO (m a) ,Serial IO (m (a -> a)), Serial IO (a -> m a)
     )
  => Series IO (m a) -> TestTree
testMono ms = testGroup "Monad laws"
  [ Applicative.testMono ms
  , testProperty "(m >>= f) >>= g ≡ m (f >=> g)"
  $ associativitySum ms (series :: Series IO (a -> m a))
                        (series :: Series IO (a -> m a))
  ]

-- | @tasty@ 'TestTree' for 'Monad' laws. Monomorphic product 'Series'.
testMonoExhaustive
  :: forall m a .
     ( Applicative m, Monad m
     , Eq a, Eq (m a), Eq (m (m a))
     , Show a, Show (m a), Show (m (a -> a))
     , Serial Identity a, Serial Identity (m a)
     , Serial IO a, Serial IO (a -> a)
     , Serial IO (m a) ,Serial IO (m (a -> a)), Serial IO (a -> m a)
     )
  => Series IO (m a) -> TestTree
testMonoExhaustive ms = testGroup "Monad laws"
  [ Applicative.testMono ms
  , testProperty "(m >>= f) >>= g ≡ m (f >=> g)"
    $ associativity ms (series :: Series IO (a -> m a))
                       (series :: Series IO (a -> m a))
  ]
