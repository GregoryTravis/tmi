{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Tmi
( tmiMain ) where

import Util

class V v where
  type D v
  type instance D a = a
  (.+) :: v -> D v -> v

instance V Int where
  --type D Int = Int
  _ .+ x = x

data DList a = DListUpdate Int a | DListCons a | NDListUpdate Int (D a)
instance V a => V [a] where
  type D [a] = DList a
  xs .+ (NDListUpdate i dx) = take i xs ++ [x .+ dx] ++ drop (i+1) xs
    where x = xs !! i
  xs .+ (DListUpdate i x) = take i xs ++ [x] ++ drop (i+1) xs
  xs .+ (DListCons x) = x : xs

tmiMain = do
  msp $ [1::Int, 2, 3] .+ (DListUpdate 1 (20::Int))
  msp $ [1::Int, 2, 3] .+ (DListCons (10::Int))
  msp $ [[5::Int], [1, 2, 3], [6]] .+ DListUpdate 1 [3, 2, 1]
  msp $ [[5::Int], [1, 2, 3], [6]] .+ NDListUpdate 1 (DListUpdate 2 30)
  msp "hihi"
