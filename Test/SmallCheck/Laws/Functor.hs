{-# LANGUAGE FlexibleContexts #-}
module Test.SmallCheck.Laws.Functor where

import Data.Functor.Identity (Identity)
import Control.Applicative (liftA3)
import Test.SmallCheck (Property, over)
import Test.SmallCheck.Series (Serial, CoSerial, Series, coseries)

fmapIdentity
  :: (Eq (f a), Monad m, Show (f a), Functor f)
  => Series m (f a) -> Property m
fmapIdentity s = over s $ \x -> fmap id x == x

-- TODO: This tends to combinatorial explosion too easily
fmapCompose
  :: ( Eq (f (f a)), Functor f, Show a, Show (f a)
     , Serial Identity a , Serial Identity (f a)
     , CoSerial m a , CoSerial m (f a)
     )
  => Series m (f a) -> Property m
fmapCompose s = over s' $ \(x,f,g) -> fmap (f . g) x == (fmap f . fmap g) x
  where
    s' = liftA3 (,,) s (coseries s) (coseries s)
