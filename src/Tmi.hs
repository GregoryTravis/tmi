{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module Tmi
( tmiMain ) where

import Data.Char (chr, ord)

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

(.*) :: F db dc -> F da db -> F da dc
F { for = forBC, rev = revBC, drev = drevBC } .* F { for = forAB, rev = revAB, drev = drevAB } = F { for = forAC, rev = revAC, drev = drevAC }
  where forAC = forBC . forAB
        -- revAC :: c -> a -> a
        revAC c a = revAB b a
          where b = revBC c b'
                b' = forAB a
        -- drevAC :: dc -> a -> da
        drevAC dc a = drevAB db a
          where db = drevBC dc b
                b = forAB a

data W = W { ints :: [Int], strings :: [String] }
  deriving Show
data DW di ds = DWInts di | DWStrings ds
--instance (Delta da, (V da) ~ [Int]) => Delta (WIntsDelta da) where
instance (V di ~ [Int], V ds ~ [String], Delta di, Delta ds) => Delta (DW di ds) where
  type V (DW di ds) = W
  w .+ DWInts di = w { ints = (ints w) .+ di }

-- _ints :: F W [Int]
-- _ints = F { for = ints
--           , rev = \x w -> w { ints = x }
--           , drev = \dx w -> w { ints = (ints w) .+ dx } }

data DString = Prepend String | Append String
  deriving Show
instance Delta DString where
  type V DString = String
  s .+ (Prepend s') = s' ++ s
  s .+ (Append s') = s ++ s'

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

(!!-) :: F da (DList db) -> Int -> F da db
xs !!- i = arrIndex i .* xs

encoder :: Int -> F DString DString
encoder n = F { for, rev, drev }
  where for s = map fc s
        rev = undefined
        drev (Prepend prefix) _ = Prepend (map rc prefix)
        fc c = chr (ord c + n)
        rc c = chr (ord c - n)

encoderF n x = encoder n .* x

ooo = (DWInts (DListMod 1 (Full 20)))

tmiMain = do
  let w = W { ints = [1, 2, 3, 4], strings = ["asdf", "zxcv", "qwer"] }
  msp $ [1, 2, 3, 4] .+ DListMod 1 (Full 20)
  msp $ [[1, 2, 3, 4]] .+ DListMod 0 (DListMod 1 (Full 21))
  msp $ "asdf" .+ Prepend "zzz"

  -- Doesn't work because of the unspecified string type
  -- msp $ w .+ DWInts (DListMod 1 (Full 20))
  msp $ w .+ ((DWInts (DListMod 1 (Full 20))) :: DW (DList (Full Int)) (DList DString))
  --msp $ encoder 3 "asdf" .+ Prepend "zzz"
  msp "hihi"
