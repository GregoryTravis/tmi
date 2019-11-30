{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  apply xs (Delete i) = take i xs ++ drop (i+1) xs

instance Delta (ListConsDelta a) where
  type V (ListConsDelta a) = [a]
  apply xs (Cons x) = x:xs
  apply xs (Snoc x) = xs ++ [x]

data StringDelta = Prepend String | Append String

instance Delta StringDelta where
  type V StringDelta = String
  apply s (Prepend s') = s' ++ s
  apply s (Append s') = s ++ s'

-- Goal: modify an array of strings: change the second element, and change it by prepending to it
-- s: string
-- ds: StringDelta
data Foo s ds = Update' Int ds

-- hupdate :: [String] -> Foo String StringDelta -> [String]
-- -- works
-- hupdate ss (Update' i ds) = take i ss ++ [newS] ++ drop (i+1) ss
--   where newS = apply (ss !! i) ds
-- bag = hupdate ["asdf", "zxcv", "qwer"] (Update' 1 (Prepend "aa"))


instance Delta ds => Delta (Foo s ds) where
  type V (Foo s ds) = [V ds] -- crucial
  apply ss (Update' i ds) = take i ss ++ [newS] ++ drop (i+1) ss
    where newS = apply (ss !! i) ds

--hag = apply ["asdf"::String, "zxcv", "qwer"] (Update' 1 (Prepend "aa"))

--instance Delta (Foo String StringDelta) where
--  type V (Foo String StringDelta) = [String]
--  apply ss (Update' i ds) = take i ss ++ [newS] ++ drop (i+1) ss
--    where newS = apply (ss !! i) ds

--hag = apply ["asdf"::String, "zxcv", "qwer"] (Update' 1 (Prepend "aa"))
----hag = apply (["asdf"::String, "zxcv", "qwer"] :: V (Foo _ StringDelta)) (Update' 1 (Prepend "aa"))


--instance Delta (Foo s ds) where
--  type V (Foo s ds) = [s]
--  apply ss (Update' i ss') = take i ss -- ++ ([apply (ss !! i) ss'] :: _) ++ drop (i+1) ss
--  --apply s (Update' i s') = take i s ++ (s !! i) ++ drop (i+1) s
--    where q = 1
--          a0 = ss' :: _
--          a1 = ss :: _
--          a2 = (ss !! 1) :: _
--          a3 = ((apply :: V StringDelta -> StringDelta -> V StringDelta (ss !! 1) ss') :: _
--          a4 = (apply :: _) "hey" (Prepend "ss")

typeFamilyDeltaDemo = do
  msp $ apply [1, 2, 3] (Insert 1 10)
  msp $ apply [1, 2, 3] (Delete 1)
  msp $ apply [1, 2, 3] (Update 1 10)
  msp $ apply [1, 2, 3] (Cons 0)
  msp $ apply [1, 2, 3] (Snoc 4)
  msp $ apply "hey" (Prepend "ss")
  msp $ apply "hey" (Append "tt")
  msp $ apply ["one", "two", "three"] (Update' 1 (Prepend "sh"))
  msp $ apply ["asdf", "zxcv", "qwer"] (Update' 1 (Append "aa"))
  msp "hoo"
