module Main where

import Data.Monoid (Sum(..), Product(..))

import Test.DumbCheck (Series, Serial(series))
import Test.Tasty (TestTree, defaultMain, testGroup)

import qualified Test.Tasty.Laws.Applicative as Applicative
import qualified Test.Tasty.Laws.Functor as Functor
import qualified Test.Tasty.Laws.Monad as Monad
import qualified Test.Tasty.Laws.Monoid as Monoid

main :: IO ()
main = defaultMain $ testGroup "Laws"
     [ monoidTests
     , functorTests
     , applicativeTests
     , monadTests
     ]

monoidTests :: TestTree
monoidTests = testGroup "Monoid"
  [ testGroup "Product"
     [ testGroup "Int"
      [ Monoid.test (series :: Series (Product Int)) ]
    , testGroup "Integer"
      [ Monoid.test (series :: Series (Product Integer)) ]
    , testGroup "Float"
      [ Monoid.test (series :: Series (Product Float)) ]
    ]
  , testGroup "Exhausitive Sum"
    [ testGroup "Int"
      [ Monoid.testExhaustive (series :: Series (Sum Int)) ]
    , testGroup "Integer"
      [ Monoid.testExhaustive (series :: Series (Sum Integer)) ]
    , testGroup "Float"
      [ Monoid.testExhaustive (series :: Series (Sum Float)) ]
    ]
  ]

functorTests :: TestTree
functorTests = testGroup "Functor"
  [ testGroup "Maybe"
    [ testGroup "Unit"
      [ Functor.testUnit (series :: Series (Maybe ())) ]
    , testGroup "Int"
      [ Functor.test (series :: Series (Maybe Int)) ]
    , testGroup "Char"
      [ Functor.test (series :: Series (Maybe Char)) ]
    , testGroup "Bool"
      [ Functor.testExhaustive (series :: Series (Maybe Bool)) ]
    ]
  , testGroup "[]"
    [ testGroup "Unit"
      [ Functor.testUnit (series :: Series [()]) ]
    , testGroup "Bool"
      [ Functor.test (series :: Series [Bool]) ]
    , testGroup "Int"
      [ Functor.testExhaustive (series :: Series [Int]) ]
    ]
  ]

applicativeTests :: TestTree
applicativeTests = testGroup "Applicative"
  [ testGroup "Maybe"
    [ testGroup "Unit"
      [ Applicative.testUnit (series :: Series (Maybe ())) ]
    , testGroup "Bool"
      [ Applicative.testExhaustive (series :: Series (Maybe Bool)) ]
    , testGroup "Int"
      [ Applicative.test (series :: Series (Maybe Int)) ]
    , testGroup "Float"
      [ Applicative.test (series :: Series (Maybe Float)) ]
    ]
  , testGroup "[]"
    [ testGroup "Unit"
      [ Applicative.testUnit (series :: Series [()]) ]
    , testGroup "Bool"
      [ Applicative.test (series :: Series [Bool]) ]
    ]
  ]

monadTests :: TestTree
monadTests = testGroup "Monad"
  [ testGroup "Maybe"
    [ testGroup "Unit"
      [ Monad.testUnit (series :: Series (Maybe ())) ]
    , testGroup "Bool"
      [ Monad.testExhaustive (series :: Series (Maybe Bool)) ]
    , testGroup "Int"
      [ Monad.test (series :: Series (Maybe Int)) ]
    ]
  , testGroup "[]"
    [ testGroup "Unit"
      [ Monad.testUnit (series :: Series [()]) ]
    , testGroup "Bool"
      [ Monad.test (series :: Series [Bool]) ]
    ]
  ]
