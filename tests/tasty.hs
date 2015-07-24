{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Data.Monoid (Sum(..), Product(..))
import Data.Proxy

import Test.SmallCheck.Series (Serial(series), Series)
import Test.Tasty (TestTree, defaultMain, testGroup)

import Test.Tasty.SmallCheck.Laws.Functor
import Test.Tasty.SmallCheck.Laws.Monoid

main :: IO ()
main = defaultMain $ testGroup "Laws" [monoidTests, functorTests]

monoidTests :: TestTree
monoidTests = testGroup "Monoid"
  [ testGroup "Sum"
    [ testGroup "Int"
      [ testMonoid $ Sum <$> (series :: Series IO Int) ]
    , testGroup "Integer"
      [ testMonoid $ Sum <$> (series :: Series IO Integer) ]
    , testGroup "Float"
      [ testMonoid $ Sum <$> (series :: Series IO Float) ]
    ]
  , testGroup "Product"
    [ testGroup "Int"
      [ testMonoid $ Product <$> (series :: Series IO Int) ]
    , testGroup "Integer"
      [ testMonoid $ Product <$> (series :: Series IO Integer) ]
    , testGroup "Float"
      [ testMonoid $ Product <$> (series :: Series IO Float) ]
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
