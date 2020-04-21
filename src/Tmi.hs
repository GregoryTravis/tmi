{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Tmi
( tmiMain ) where

import Util

class V v where
  type D v
  (.+) :: v -> D v -> v

data DNum n = DNumAdd n | DNumSub n
instance Num n => V n where
  type D n = DNum n
  x .+ (DNumAdd dx) = x + dx
  x .+ (DNumSub dx) = x - dx

-- data DList a = DListUpdate Int a | DListCons a
-- instance V [a] where
--   type D [a] = DList a
--   xs .+ (DListUpdate i x) = replaceAt i x
--     where replaceAt i x xs = take i xs ++ [x] ++ drop (i+1) xs
--   xs .+ (DListCons x) = x : xs

tmiMain = do
  msp $ 3 .+ (DNumAdd 12)
  -- msp $ [1, 2, 3] .+ (DListUpdate 1 20)
  -- msp $ [1, 2, 3] .+ (DListCons 10)
  msp "hihi"
