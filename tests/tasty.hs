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

import Test.SmallCheck.Series (Serial(series))
import Test.Tasty (TestTree, defaultMain, testGroup)

import Test.Tasty.SmallCheck.Laws.Applicative
import Test.Tasty.SmallCheck.Laws.Functor
import Test.Tasty.SmallCheck.Laws.Monad
import Test.Tasty.SmallCheck.Laws.Monoid

main :: IO ()
main = defaultMain $ testGroup "Laws"
     [ monoidTests
     , functorTests
     , applicativeTests
     , monadTests
     ]

monoidTests :: TestTree
monoidTests = testGroup "Monoid"
  [ testGroup "Sum"
    [ testGroup "Int"
      [ testMonoid (Proxy :: Proxy (Sum Int)) ]
    , testGroup "Integer"
      [ testMonoid (Proxy :: Proxy (Sum Integer)) ]
    , testGroup "Float"
      [ testMonoid (Proxy :: Proxy (Sum Float)) ]
    ]
  , testGroup "Product"
     [ testGroup "Int"
      [ testMonoid (Proxy :: Proxy (Product Int)) ]
    , testGroup "Integer"
      [ testMonoid (Proxy :: Proxy (Product Integer)) ]
    , testGroup "Float"
      [ testMonoid (Proxy :: Proxy (Product Float)) ]
    ]
  ]

functorTests :: TestTree
functorTests = testGroup "Functor"
  [ testGroup "Maybe"
    [ testGroup "Int"
      [ testFunctor (Proxy :: Proxy (Maybe Int)) ]
    , testGroup "Char"
      [ testFunctor (Proxy :: Proxy (Maybe Char)) ]
    ]
  , testGroup "[]"
    [ testGroup "Bool"
      [ testFunctor (Proxy :: Proxy [Bool]) ]
    , testGroup "Int"
      [ testFunctor (Proxy :: Proxy [Int]) ]
    ]
  ]

applicativeTests :: TestTree
applicativeTests = testGroup "Applicative"
  [ testGroup "Maybe"
    [ testGroup "Int"
      [ testApplicative (Proxy :: Proxy (Maybe Int)) ]
    , testGroup "Float"
      [ testApplicative (Proxy :: Proxy (Maybe Float)) ]
    ]
  , testGroup "[]"
    [ testGroup "Bool"
      [ testApplicative (Proxy :: Proxy [Bool]) ]
    , testGroup "Char"
      [ testApplicative (Proxy :: Proxy [Char]) ]
    ]
  ]

monadTests :: TestTree
monadTests = testGroup "Monad"
  [ testGroup "Maybe"
    [ testGroup "()"
      [ testMonad (Proxy :: Proxy (Maybe ())) ]
    , testGroup "Int"
      [ testMonad (Proxy :: Proxy (Maybe Int)) ]
    ]
  , testGroup "[]"
    [ testGroup "()"
      [ testMonad (Proxy :: Proxy [()]) ]
    ]
  ]

instance (Monad m, Serial m a) => Serial m (Sum a) where
    series = Sum <$> series

instance (Monad m, Serial m a) => Serial m (Product a) where
    series = Product <$> series
