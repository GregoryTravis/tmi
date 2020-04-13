{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Existential
( existentialMain ) where

import Util

class DeltaOf a da | da -> a where 
  (.+) :: a -> da -> a

data Delta a = forall da. (DeltaOf a da) => Delta da

instance DeltaOf a (Delta a) where
  x .+ (Delta dx) = x .+ dx

data DIntAdd = DIntAdd Int
instance DeltaOf Int DIntAdd where 
  i .+ (DIntAdd di) = i + di

data DIntSub = DIntSub Int
instance DeltaOf Int DIntSub where 
  i .+ (DIntSub di) = i - di

data DDoubleAdd = DDoubleAdd Double
instance DeltaOf Double DDoubleAdd where
  d .+ (DDoubleAdd dd) = d + dd

data DList a = DListMod Int a | DListCons a
instance DeltaOf [a] (DList a) where
  xs .+ DListMod i x = take i xs ++ [x] ++ drop (i+1) xs
  xs .+ DListCons x = x : xs

-- r record, f field type
type BiField r f = (r -> f, f -> r -> r)

data W = W { anInt :: Int
           , aDouble :: Double
           , aList :: [Int] }
  deriving Show

-- Lensery
-- data BiW = BiW { biAnInt :: BiField W Int
--                , biADouble :: BiField W Double
--                , biAList :: BiField W [Int] }
-- biW = BiW { biAnInt = (anInt, \i w -> w { anInt = i })
--           , biADouble = (aDouble, \d w -> w { aDouble = d })
--           , biAList = (aList, \l w -> w { aList = l }) }
biAnInt :: BiField W Int
biAnInt = (anInt, \i w -> w { anInt = i })
biADouble :: BiField W Double
biADouble = (aDouble, \d w -> w { aDouble = d })
biAList :: BiField W [Int]
biAList = (aList, \l w -> w { aList = l })
--appLens :: BiField r f -> r ->
readLens :: BiField r f -> r -> f
readLens = fst
writeLens :: BiField r f -> f -> r -> r
writeLens = snd
modLens :: BiField r f -> (f -> f) -> r -> r
modLens bif modder r = writeLens bif (modder $ readLens bif r) r
dModLens :: BiField r f -> Delta f -> r -> r
dModLens bif df = modLens bif (.+ df)

data DW = DAnInt (Delta Int) | DADouble (Delta Double) | DAList (Delta [Int])

instance DeltaOf W DW where
  w .+ DAnInt di = dModLens biAnInt di w
  w .+ DADouble dd = dModLens biADouble dd w
  w .+ DAList dxs = dModLens biAList dxs w
  --w@(W { anInt = i }) .+ DAnInt d = w { anInt = (i .+ d) }
  --w@(W { aDouble = d }) .+ DADouble dd = w { aDouble = (d .+ dd) }
  --w@(W { aList = xs }) .+ DAList dxs = w { aList = (xs .+ dxs) }

-- Can generate this for each field of W
_dGeneric :: (DeltaOf a da, DeltaOf r dr) => ((Delta a) -> dr) -> r -> da -> r
_dGeneric lifter w da = w .+ (lifter (Delta da))
-- DAnInt :: Delta Int -> DW

_dAnInt :: DeltaOf Int di => W -> di -> W
--_dAnInt w di = w .+ (DAnInt (Delta di))
_dAnInt = _dGeneric DAnInt
_dADouble :: DeltaOf Double dd => W -> dd -> W
--_dADouble w dd = w .+ (DADouble (Delta dd))
_dADouble = _dGeneric DADouble
_dAList :: DeltaOf [Int] dl => W -> dl -> W
--_dAList w dl = w .+ (DAList (Delta dl))
_dAList = _dGeneric DAList

-- Alternately, lift (aka reverse transform) the delta separately
-- Also generated automatically for each field in W
toDAnInt :: Delta Int -> Delta W
toDAnInt di = Delta (DAnInt (Delta di))
toDADouble dd = Delta (DADouble (Delta dd))
toDAList dl = Delta (DAList (Delta dl))
  -- let dw :: DW
  --     dw = DAnInt (Delta di)
  --  in (Delta dw)
_dAnInt' :: DeltaOf Int di => W -> di -> W
_dAnInt' w di = w .+ (toDAnInt (Delta di))
_dADouble' :: DeltaOf Double dd => W -> dd -> W
_dADouble' w dd = w .+ (toDADouble (Delta dd))
_dAList' w dd = w .+ (toDAList (Delta dd))

existentialMain = do
  let w :: W
      w = W { anInt = 10
            , aDouble = 3.3
            , aList = [1, 2, 3] }
  --msp $ 3 .+ DIntAdd 2
  --msp $ 3 .+ DIntSub 2
  --msp $ w .+ DAnInt (Delta (DIntAdd 6))
  --msp $ w .+ DADouble (Delta (DDoubleAdd 0.2))
  ---- msp $ _dAnInt w (Delta (DIntAdd 5))
  ----msp $ foo w (DIntAdd 5)
  --let x :: DIntAdd
  --    x = DIntAdd 7
  --    x' :: Delta Int
  --    x' = Delta x
  --    hoo :: DeltaOf a da => da -> Delta a
  --    hoo = Delta
  --    x'' = hoo (DIntAdd 8)
  --    --vree :: DeltaOf Int di => W -> di -
  --    -- vree :: DeltaOf Int di => W -> di -> W
  --    -- vree w di = _dAnInt w (Delta di)
  ---- msp $ _dAnInt w x'
  ---- msp $ _dAnInt w x''
  msp $ _dAnInt w (DIntAdd 9)
  msp $ _dAnInt w (DIntSub 2)
  msp $ _dADouble w (DDoubleAdd 0.1)
  msp $ _dAnInt' w (DIntAdd 9)
  msp $ _dAnInt' w (DIntSub 2)
  msp $ _dADouble' w (DDoubleAdd 0.1)
  msp $ [1, 2, 3] .+ (DListMod 1 20)
  msp $ [1, 2, 3] .+ (DListCons 7)
  msp "hihi"
