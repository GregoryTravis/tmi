{-# LANGUAGE TypeFamilies #-}
module TFDelta (typeFamilyDeltaDemo) where

import Util

-- instance ArrayElem Bool where
--     data Array Bool = BoolArray BitVector
--     index (BoolArray ar) i = indexBitVector ar i

class Delta d where
  data V d :: *
  apply :: V d -> d -> V d

data ListIndexDelta a = Insert Int a | Delete Int | Update Int a deriving Show

instance Delta (ListIndexDelta a) where
  data V (ListIndexDelta a) = L [a] deriving Show
  apply (L xs) (Insert i x) = L (take i xs ++ [x] ++ drop i xs)
  apply (L xs) (Update i x) = L (take i xs ++ [x] ++ drop (i+1) xs)

data ListConsDelta a = Cons a | Snoc a

instance Delta (ListConsDelta a) where
  data V (ListConsDelta a) = L' [a] deriving Show
  apply (L' xs) (Cons x) = L' (x:xs)
  apply (L' xs) (Snoc x) = L' (xs ++ [x])

class D2 d where
  type V2 d
  apply2 :: V2 d -> d -> V2 d

instance D2 (ListIndexDelta a) where
  type V2 (ListIndexDelta a) = [a]
  apply2 xs (Insert i x) = take i xs ++ [x] ++ drop i xs
  apply2 xs (Update i x) = take i xs ++ [x] ++ drop (i+1) xs

instance D2 (ListConsDelta a) where
  type V2 (ListConsDelta a) = [a]
  apply2 xs (Cons x) = x:xs
  apply2 xs (Snoc x) = xs ++ [x]

typeFamilyDeltaDemo = do
  msp $ apply (L [1, 2, 3]) (Insert 1 10)
  msp $ apply (L [1, 2, 3]) (Update 1 10)
  msp $ apply (L' [1, 2, 3]) (Cons 0)
  msp $ apply (L' [1, 2, 3]) (Snoc 4)
  msp $ apply2 [1, 2, 3] (Insert 1 10)
  msp $ apply2 [1, 2, 3] (Update 1 10)
  msp $ apply2 [1, 2, 3] (Cons 0)
  msp $ apply2 [1, 2, 3] (Snoc 4)
  msp "hoo"
