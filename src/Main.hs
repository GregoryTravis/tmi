{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
module Main where

import Delta
import Exp
import Tmi
import Util

class Delta a d where
  (.+) :: a -> d -> a
  (.-) :: a -> a -> d

data ListDelta a = Insert Int a | Delete Int
deriving instance Show a => Show (ListDelta a)

instance Delta [a] (ListDelta a) where
  xs .+ (Insert i x) = (take i xs) ++ [x] ++ (drop i xs)
  (.-) = undefined  -- slow

main = do
  msp $ ["a", "b", "c"] .+ Insert 2 "d"
  msp "hi"
  -- deltaDemo
  -- expDemo
  -- oldWebDemo
  -- bankProcess
