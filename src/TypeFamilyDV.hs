{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module TypeFamilyDV
( typeFamilyDVMain ) where

import Control.Monad.State
import Data.Char (chr, ord)

import Util

class Delta d where
  type V d
  (.+) :: V d -> d -> V d

data Full a = Full a
  deriving Show
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

-- Field lenses
intsL :: V dli ~ [Int] => F (DWInts dli) dli
intsL = F { for, rev, drev }
  where for = ints
        rev = \ints w -> w { ints }
        drev di _ = DWInts di
stringsL :: V dli ~ [String] => F (DWStrings dli) dli
stringsL = F { for, rev, drev }
  where for = strings
        rev = \strings w -> w { strings }
        drev di _ = DWStrings di

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

---- Monad

type TMI w a = StateT w IO a

tmiRun :: w -> TMI w a -> IO (a, w)
tmiRun world action = do
  (x, world') <- runStateT action world
  -- TODO world or world'?
  --let result = (for x) world
  return (x, world')

infix 4 <-+
(<-+) :: Delta da => F da db -> db -> TMI (V da) ()
dest <-+ src = do
  w <- get
  let w' = w .+ drev dest src w
  --let w' = apply w (valDWrite w dest src)
  put w'

infix 4 <--
-- I think this should be right but it's not
-- (<--) :: (Delta da, Delta db) => Fun (V da) (V db) da db -> V db -> TMI (V da) ()
--(<--) :: Delta da => F da db -> db -> TMI (V da) ()
dest <-- src = dest <-+ (Full src)

tmiRunShow world action = do
  (x, world') <- tmiRun world action
  msp world'
  msp x

---- main

anAction :: TMI W ()
anAction = do
  encoderF 2 (stringsL !!- 1) <-+ (Prepend "sss")
  return ()

typeFamilyDVMain = do
  let w = W { ints = [1, 2, 3, 4], strings = ["asdf", "zxcv", "qwer"] }

  tmiRunShow w anAction

  -- This all works
  --msp $ [1, 2, 3, 4] .+ DListMod 1 (Full 20)
  --msp $ [[1, 2, 3, 4]] .+ DListMod 0 (DListMod 1 (Full 21))
  --msp $ "asdf" .+ Prepend "zzz"

  ---- Doesn't work because of the unspecified string type
  --msp $ w .+ DWInts (DListMod 1 (Full 20))
  --msp $ w .+ DWStrings (DListMod 1 (Prepend "qqqq"))
  --let edeltas :: [EDelta W]
  --    edeltas = [EDelta $ DWInts (DListMod 1 (Full 20)), EDelta $ DWStrings (DListMod 1 (Prepend "qqqq"))]
  --    --edeltas = [DWInts (DListMod 1 (Full 20)), DWStrings (DListMod 1 (Prepend "qqqq"))]
  --msp $ w .+ Compound edeltas

  --msp $ w .+ drev intsL (DListMod 1 (Full 20)) w
  --msp $ w .+ drev stringsL (DListMod 1 (Prepend "jjj")) w
  --msp $ w .+ (drev (arrIndex 1 .* stringsL) (Prepend "uuu") w)
  --msp $ w .+ (drev (stringsL !!- 1) (Prepend "vvv") w)
  --msp $ w .+ (drev (encoderF 2 (stringsL !!- 1)) (Prepend "sss") w)

  msp "hihi"
