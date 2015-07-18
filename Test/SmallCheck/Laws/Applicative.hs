{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.SmallCheck.Laws.Applicative where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative, (<*>), pure)
#endif
import Data.Functor.Identity (Identity)
import Test.SmallCheck (Property, over)
import Test.SmallCheck.Series (Serial, Series)
import Test.SmallCheck.Series.Utils (zipLogic, zipLogic3)

identity
  :: (Eq (f a), Monad m, Show (f a), Applicative f)
  => Series m (f a) -> Property m
identity xs = over xs $ \x -> (pure id <*> x) == x

composition
  :: ( Eq (f b)
     , Monad m
     , Show (f c)
     , Show (f (a -> b))
     , Show (f (c -> a))
     , Applicative f
     )
  => Series m (f (c -> a))
  -> Series m (f c)
  -> Series m (f (a -> b))
  -> Property m
composition vs ws us = over (zipLogic3 vs ws us) $ \(v,w,u) ->
    (pure (.) <*> u <*> v <*> w) == (u <*> (v <*> w))

homomorphism
  :: forall m a b .
     ( Monad m
     , Eq b
     , Show a, Show b
     , Serial Identity a
     , Serial Identity b
     )
  => Series m a -> Series m (a -> b) -> Property m
homomorphism xs fs = over (zipLogic xs fs) $ \(x,f) ->
    (pure f <*> (pure x :: Maybe a)) == pure (f x)

interchange
  :: ( Eq (f b)
     , Monad m
     , Show a
     , Show (f (a -> b))
     , Applicative f
     )
  => Series m a -> Series m (f (a -> b)) -> Property m
interchange ys us = over (zipLogic ys us) $ \(y,u) ->
    (u <*> pure y) == (pure ($ y) <*> u)
