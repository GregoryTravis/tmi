{-# LANGUAGE FlexibleInstances #-}
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

instance Delta da => Delta (DList da) where
  type V (DList da) = [V da]
  xs .+ DListMod i dx = take i xs ++ [x .+ dx] ++ drop (i+1) xs
    where x = xs !! i
  xs .+ DListCons x = x : xs

data DList da = DListMod Int da | DListCons (V da)
-- instance DeltaOf [a] (DList a) where

-- arrIndex :: Int -> Fun [a] a (ListIndexDelta da) da
-- arrIndex i = Fun { for, rev, drev }
--   where for = (!!i)
--         rev x xs = take i xs ++ [x] ++ drop (i+1) xs
--         drev dx _ = Update i dx

tmiMain = do
  let w = W { ints = [1, 2, 3, 4] }
  msp $ [1, 2, 3, 4] .+ DListMod 1 (Full 20)
  msp "hihi"
