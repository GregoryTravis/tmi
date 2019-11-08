{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module SimpleDelta
( simpleDeltaDemo
) where

import qualified Data.Map.Strict as M
import qualified Debug.Trace as TR

import Util 

data Fun a b da db = Fun
  { for :: a -> b
  , rev :: b -> a -> a
  , drev :: db -> a -> da }

data ListIndexDelta a = LISet Int a

data W = W
  { ints :: [Int] }
  deriving (Eq, Show)
_ints :: [Int] -> W -> W
_ints xs w = w { ints = xs }

data WDints dints = WDints dints

class Delta a da where
  apply :: a -> da -> a

instance Delta [a] (ListIndexDelta a) where
  apply xs (LISet i x) = take i xs ++ [x] ++ drop (i+1) xs

instance Delta [Int] dw => Delta W (WDints dw) where
  apply w@(W { ints = ints }) (WDints dw) = w { ints = (apply ints dw) }

world = W
  { ints = [0, 1, 2, 3, 4] }

simpleDeltaDemo = do
  msp $ _ints [3, 4, 5] world
  msp $ apply [4::Int, 5, 6] (LISet 1 (50::Int))
  msp $ apply (W { ints = [4::Int, 5, 6] }) (WDints (LISet 1 (50::Int)))
  msp "shi"
