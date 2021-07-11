{-# LANGUAGE
    ExistentialQuantification
  #-}

module Monies
( moniesMain ) where

import Control.Applicative
import Control.Monad (join)

import Util

data B a = B a deriving Show

instance Functor B where
  fmap f (B a) = B (f a)

instance Applicative B where
  pure = B
  liftA2 f (B a) (B b) = B (f a b)
-- liftA2 f x = (<*>) (fmap f x)
-- (<*>) = liftA2 id

instance Monad B where 
  B a >>= aToBb = aToBb a
  -- Hangs, why?
  -- B a >>= aToBb = join $ fmap aToBb (B a)
  -- B a >>= aToBb = fmap aToBb (B a)
-- (>>=) :: forall a b. m a -> (a -> m b) -> m b infixl 1

data Fri a = Fri a | forall b. Bund (Fri b) (b -> Fri a)

instance Functor Fri where
  fmap f (Fri a) = Fri (f a)

instance Applicative Fri where
  pure = Fri
  liftA2 f (Fri a) (Fri b) = Fri (f a b)

instance Monad Fri where
  return a = Fri a
  (>>=) = Bund

evl :: Fri a -> a
evl (Fri a) = a
evl (Bund (Fri b) bToFriA) = evl (bToFriA b)

moniesMain = do
  msp $ fmap (+1) (B 12)
  msp $ liftA2 (+) (B 12) (B 13)
  msp $ (B 1) >>= \i -> (B (i*10))
  msp $ (B 1) >>= B . (*100)
  msp $ evl $ Fri 12
  msp $ evl $ Fri 12 >>= Fri . (*20)
  msp "hi monies"
