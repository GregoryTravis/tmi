{-# LANGUAGE ExistentialQuantification, GADTs, RecordWildCards, StandaloneDeriving,
             TypeApplications, TypeFamilies #-}

module Curry (curryMain) where

import Control.Applicative

import Util

data W = W

data F a = F (W -> a)
data R a = R (W -> a -> W)

data V a = V (F a) (R a)

instance Functor V where
  fmap = undefined

instance Applicative V where
  pure = undefined
  (<*>) = undefined
  --liftA2 :: (a -> b -> c) -> f a -> f b -> f c

inc :: V (Int -> Int)
inc = undefined

lInc :: V Int -> V Int
lInc = (inc <*>)

plus :: V (Int -> Int -> Int)
plus = undefined

plusV :: V Int -> V Int -> V Int
plusV = (<*>) . (plus <*>)

curryMain = do
  msp "curry hi"
