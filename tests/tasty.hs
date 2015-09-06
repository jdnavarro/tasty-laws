{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Data.Monoid (Sum(..), Product(..))
import Data.Proxy (Proxy(..))

import Test.SmallCheck.Series (Series, Serial(series))
import Test.Tasty (TestTree, defaultMain, testGroup)

import Test.Tasty.Laws.Applicative
import Test.Tasty.Laws.Monad
import qualified Test.Tasty.Laws.Functor as Functor
import qualified Test.Tasty.Laws.Monoid as Monoid

main :: IO ()
main = defaultMain $ testGroup "Laws"
     [ monoidTests
     , functorTests
--     , applicativeTests
--     , monadTests
     ]

instance (Monad m, Serial m a) => Serial m (Sum a) where
    series = Sum <$> series

instance (Monad m, Serial m a) => Serial m (Product a) where
    series = Product <$> series

monoidTests :: TestTree
monoidTests = testGroup "Monoid"
  [ testGroup "Product"
     [ testGroup "Int"
      [ Monoid.test (series :: Series IO (Product Int)) ]
    , testGroup "Integer"
      [ Monoid.test (series :: Series IO (Product Integer)) ]
    , testGroup "Float"
      [ Monoid.test (series :: Series IO (Product Float)) ]
    ]
  , testGroup "Exhausitive Sum"
    [ testGroup "Int"
      [ Monoid.testExhaustive (series :: Series IO (Sum Int)) ]
    , testGroup "Integer"
      [ Monoid.testExhaustive (series :: Series IO (Sum Integer)) ]
    , testGroup "Float"
      [ Monoid.testExhaustive (series :: Series IO (Sum Float)) ]
    ]
  ]

functorTests :: TestTree
functorTests = testGroup "Functor"
  [ testGroup "Maybe"
    [ testGroup "Unit"
      [ Functor.test (series :: Series IO (Maybe ())) ]
    , testGroup "Int"
      [ Functor.testMono (series :: Series IO (Maybe Int)) ]
    , testGroup "Char"
      [ Functor.testMono (series :: Series IO (Maybe Char)) ]
    , testGroup "Bool"
      [ Functor.testMonoExhaustive (series :: Series IO (Maybe Bool)) ]
    ]
  , testGroup "[]"
    [ testGroup "Unit"
      [ Functor.test (series :: Series IO [()]) ]
    , testGroup "Bool"
      [ Functor.testMono (series :: Series IO [Bool]) ]
    , testGroup "Int"
      [ Functor.testMono (series :: Series IO [Int]) ]
    ]
  ]

-- applicativeTests :: TestTree
-- applicativeTests = testGroup "Applicative"
--   [ testGroup "Maybe"
--     [ testGroup "Int"
--       [ testApplicative (Proxy :: Proxy (Maybe Int)) ]
--     , testGroup "Float"
--       [ testApplicative (Proxy :: Proxy (Maybe Float)) ]
--     ]
--   , testGroup "[]"
--     [ testGroup "Bool"
--       [ testApplicative (Proxy :: Proxy [Bool]) ]
--     , testGroup "Char"
--       [ testApplicative (Proxy :: Proxy [Char]) ]
--     ]
--   ]

-- monadTests :: TestTree
-- monadTests = testGroup "Monad"
--   [ testGroup "Maybe"
--     [ testGroup "()"
--       [ testMonad (Proxy :: Proxy (Maybe ())) ]
--     , testGroup "Int"
--       [ testMonad (Proxy :: Proxy (Maybe Int)) ]
--     ]
--   , testGroup "[]"
--     [ testGroup "()"
--       [ testMonad (Proxy :: Proxy [()]) ]
--     ]
--   ]
