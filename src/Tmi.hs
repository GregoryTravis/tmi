{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Tmi
( tmiMain ) where

import Util

class V v where
  type D v
  (.+) :: v -> D v -> v

data DList a = DListUpdate Int a | DListCons a
instance V [a] where
  type D [a] = DList a
  xs .+ (DListUpdate i x) = replaceAt i x xs
    where replaceAt i x xs = take i xs ++ [x] ++ drop (i+1) xs
  xs .+ (DListCons x) = x : xs

tmiMain = do
  msp $ [1, 2, 3] .+ (DListUpdate 1 20)
  msp $ [1, 2, 3] .+ (DListCons 10)
  msp "hihi"
