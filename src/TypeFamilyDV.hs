{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module TypeFamilyDV
( typeFamilyDVMain ) where

import Data.Char (chr, ord)

import Util

class Delta d where
  type V d
  (.+) :: V d -> d -> V d

data Full a = Full a
instance Delta (Full a) where
  type V (Full a) = a
  x .+ Full x' = x'

--data EDelta a = forall da. (Delta da, V da ~ a) => EDelta (a -> a)
--data EDelta a = forall da. (Delta da, V da ~ a) => EDelta (a -> a)
data EDelta a = forall da. (Delta da, V da ~ a) => EDelta da
instance Delta (EDelta a) where
  type V (EDelta a) = a
  x .+ EDelta dx = x .+ dx

data Compound a = Compound [EDelta a]

instance Delta (Compound a) where
  type V (Compound a) = a
  -- TODO should be a fold
  x .+ Compound [] = x
  x .+ Compound (dx:dxs) = (x .+ dx) .+ Compound dxs

-- ok
--data Deltaa a = forall da. (DeltaOf a da) => Deltaa da | Fulll a
-- ok
-- data Deltaa a = forall da. (Delta da, V da ~ a) => Deltaa da | Fulll a
--not ok, I guess cuz it's missing the secret existential
--data Deltaa a = forall da. (Delta da, V da ~ a) => Fulll a
-- class DeltaOf a da | da -> a where 
--   (..+) :: a -> da -> a

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
data DWInts di = DWInts di
--instance (Delta da, (V da) ~ [Int]) => Delta (WIntsDelta da) where
instance (V di ~ [Int], Delta di) => Delta (DWInts di) where
  type V (DWInts di) = W
  w .+ DWInts di = w { ints = (ints w) .+ di }

data DWStrings ds = DWStrings ds
instance (V ds ~ [String], Delta ds) => Delta (DWStrings ds) where
  type V (DWStrings ds) = W
  w .+ DWStrings ds = w { strings = (strings w) .+ ds }

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

typeFamilyDVMain = do
  let w = W { ints = [1, 2, 3, 4], strings = ["asdf", "zxcv", "qwer"] }
  msp $ [1, 2, 3, 4] .+ DListMod 1 (Full 20)
  msp $ [[1, 2, 3, 4]] .+ DListMod 0 (DListMod 1 (Full 21))
  msp $ "asdf" .+ Prepend "zzz"

  -- Doesn't work because of the unspecified string type
  msp $ w .+ DWInts (DListMod 1 (Full 20))
  msp $ w .+ DWStrings (DListMod 1 (Prepend "qqqq"))
  let edeltas :: [EDelta W]
      edeltas = [EDelta $ DWInts (DListMod 1 (Full 20)), EDelta $ DWStrings (DListMod 1 (Prepend "qqqq"))]
      --edeltas = [DWInts (DListMod 1 (Full 20)), DWStrings (DListMod 1 (Prepend "qqqq"))]
  msp $ w .+ Compound edeltas

  --msp $ w .+ ((DWInts (DListMod 1 (Full 20))) :: DW (DList (Full Int)))
  --msp $ encoder 3 "asdf" .+ Prepend "zzz"
  msp "hihi"
