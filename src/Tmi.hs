{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Tmi
( tmiMain ) where

import Util

class Delta d where
  type V d
  (.+) :: V d -> d -> V d

data Full a = Full a
instance Delta (Full a) where
  type V (Full a) = a
  x .+ Full x' = x'

data F da db = F
  { for :: V da -> V db
  , rev :: V db -> V da -> V da
  , drev :: db -> V da -> da }

data W = W { ints :: [Int] }

-- _ints :: F W [Int]
-- _ints = F { for = ints
--           , rev = \x w -> w { ints = x }
--           , drev = \dx w -> w { ints = (ints w) .+ dx } }

data DList da = DListMod Int da | DListCons (V da)
instance Delta da => Delta (DList da) where
  type V (DList da) = [V da]
  xs .+ DListMod i dx = take i xs ++ [x .+ dx] ++ drop (i+1) xs
    where x = xs !! i
  xs .+ DListCons x = x : xs

arrIndex :: Int -> F (DList da) da
arrIndex i = F { for, rev, drev }
  where for = (!!i)
        rev x xs = xs .+ DListMod i (Full x)
        -- Or is it this?
        -- drev dx xs = DListMod i x'
        --   where x' = (xs !! i) .+ dx
        drev dx xs = DListMod i dx

--(!!-) :: [V da] -> Int -> F (DLIst da) da
--x !!- i = composeFunFun (arrIndex' i) x

tmiMain = do
  let w = W { ints = [1, 2, 3, 4] }
  msp $ [1, 2, 3, 4] .+ DListMod 1 (Full 20)
  msp "hihi"
