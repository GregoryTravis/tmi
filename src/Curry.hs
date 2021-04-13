{-# LANGUAGE ExistentialQuantification, GADTs, RecordWildCards, StandaloneDeriving,
             TypeApplications, TypeFamilies #-}

module Curry (curryMain) where

import Control.Applicative

import Util

data W = W
data What

data F a = F (W -> a)
--data R a = R (W -> a -> W)
data R a = R (a -> What)

-- How to implement this?
-- Whatever is hidden in there is actually like (b -> a)
hmm :: R (a -> b) -> R a -> R b
hmm (R aToBToWhat) (R aToWhat) = undefined

data V a = V (F a) (R a)

instance Functor V where
  fmap = undefined

instance Applicative V where
  pure = undefined
  -- (<*>) :: V (F (a -> b))
  --            (R (a -> b)) ->
  --          V (F a)
  --            (R a) ->
  --          V (F b)
  --            (R b)
  -- (<*>) :: V (W -> (a -> b))
  --            (W -> (a -> b) -> W) ->
  --          V (W -> a)
  --            (W -> a -> W) ->
  --          V (W -> b)
  --            (W -> b -> W)
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
