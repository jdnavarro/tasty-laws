{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Data.Monoid (Sum(..))

import Test.SmallCheck.Series (Serial(series), Series)
import Test.Tasty (defaultMain, testGroup)

import Test.Tasty.SmallCheck.Laws.Monoid

main :: IO ()
main = defaultMain $ testGroup "Monoid"
  [ testGroup "Sum"
    [ testGroup "Int"
      [ testMonoid $ Sum <$> (series :: Series IO Int) ]
    ]
  ]
