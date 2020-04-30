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
  (.+) :: a -> D a -> a
  x .+ dx = unFuller dx
  fuller :: a -> D a
  unFuller :: D a -> a

data Full a = Full a

instance V Int where
  fuller x = Full x
  unFuller (Full x) = x

data Empty = Empty
  deriving Show

instance V Empty where
  fuller x = Full x
  unFuller (Full x) = x

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

tmiMain = do
  msp $ [Empty, Empty, Empty] .+ DListMod 1 (fuller Empty)
  msp $ [Empty, Empty, Empty] <-+ DListMod 1 (fuller Empty)
  msp $ [Empty, Empty, Empty] <-- [Empty, Empty]
  msp $ [1::Int, 2, 3] .+ Append [4, 5]
  msp "hihi"
