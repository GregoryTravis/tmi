{-# LANGUAGE AllowAmbiguousTypes #-}
--{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

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

data W = W { anInt :: Int
           , aDouble :: Double
           , aList :: [Int] }
  deriving Show

data DW = DAnInt (Delta Int) | DADouble (Delta Double) | DAList (Delta [Int])

instance DeltaOf W DW where
  w@(W { anInt = i }) .+ DAnInt d = w { anInt = (i .+ d) }
  w@(W { aDouble = d }) .+ DADouble dd = w { aDouble = (d .+ dd) }
  w@(W { aList = xs }) .+ DAList dxs = w { aList = (xs .+ dxs) }

-- Can generate this for each field of W
_dAnInt :: DeltaOf Int di => W -> di -> W
_dAnInt w di = w .+ (DAnInt (Delta di))
_dADouble :: DeltaOf Double dd => W -> dd -> W
_dADouble w dd = w .+ (DADouble (Delta dd))
_dAList w dl = w .+ (DAList (Delta dl))

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
