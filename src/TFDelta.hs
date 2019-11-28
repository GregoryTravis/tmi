{-# LANGUAGE TypeFamilies #-}
module TFDelta (typeFamilyDeltaDemo) where

import Util

-- instance ArrayElem Bool where
--     data Array Bool = BoolArray BitVector
--     index (BoolArray ar) i = indexBitVector ar i

data ListIndexDelta a = Insert Int a | Delete Int | Update Int a deriving Show

data ListConsDelta a = Cons a | Snoc a

class Delta d where
  type V d
  apply :: V d -> d -> V d

instance Delta (ListIndexDelta a) where
  type V (ListIndexDelta a) = [a]
  apply xs (Insert i x) = take i xs ++ [x] ++ drop i xs
  apply xs (Update i x) = take i xs ++ [x] ++ drop (i+1) xs

instance Delta (ListConsDelta a) where
  type V (ListConsDelta a) = [a]
  apply xs (Cons x) = x:xs
  apply xs (Snoc x) = xs ++ [x]

typeFamilyDeltaDemo = do
  msp $ apply [1, 2, 3] (Insert 1 10)
  msp $ apply [1, 2, 3] (Update 1 10)
  msp $ apply [1, 2, 3] (Cons 0)
  msp $ apply [1, 2, 3] (Snoc 4)
  msp "hoo"
