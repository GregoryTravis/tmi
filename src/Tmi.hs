{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Tmi
( tmiMain ) where

import Util

class V a where
  type D a
  type instance D a = Full a
  --data F a :: a -> da
  -- type F a
  -- type instance F a = Full a
  (.+) :: a -> D a -> a
  --(.+) = undefined
  x .+ dx = unFuller dx
  -- _ .+ (Full x') = x'
  fuller :: a -> D a
  unFuller :: D a -> a

data Full a = Full a

--data Full a = Full a

instance V Int where
  fuller x = Full x
  unFuller (Full x) = x

data Wah = Wah
  deriving Show
data DWah = FDWah Wah

instance V Wah where
  type D Wah = DWah
  fuller wah = FDWah wah
  unFuller (FDWah wah) = wah

-- D a == da
data DList a da = DListMod Int da | DListCons a | Prepend [a] | Append [a] | FList [a]
instance V a => V [a] where
  type D [a] = DList a (D a)
  xs .+ DListMod i dx = take i xs ++ [x .+ dx] ++ drop (i+1) xs
    where x = xs !! i
  xs .+ DListCons x = x : xs
  xs .+ Prepend xs' = xs' ++ xs
  xs .+ Append xs' = xs ++ xs'
  xs .+ (FList xs') = xs'
  fuller xs = FList xs
  unFuller (FList xs) = xs

-- This is not right -- this should be (F W a) as the first arg, but it demonstrates the right issue, for now
(<-+) :: V a => a -> D a -> a
(<-+) = (.+)

(<--) :: V a => a -> a -> a
x <-- y = x <-+ (fuller y)

arf = [1::Int, 2, 3] .+ Append [4, 5]
--arf = [1::Int, 2, 3] .+ undefined
--arf = DListMod 1 20

tmiMain = do
  msp $ [Wah, Wah, Wah] .+ DListMod 1 (fuller Wah)
  msp $ [Wah, Wah, Wah] <-+ DListMod 1 (fuller Wah)
  msp $ [Wah, Wah, Wah] <-- [Wah, Wah]
  msp $ [1::Int, 2, 3] .+ Append [4, 5]
--arf = [1::Int, 2, 3] .+ undefined
  msp "hihi"
