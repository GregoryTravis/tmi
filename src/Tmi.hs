{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Tmi
( tmiMain ) where

import Util

class V a where
  type D a
  type instance D a = Full a
  (.+) :: a -> D a -> a
  x .+ dx = unFuller dx
  fuller :: a -> D a
  unFuller :: D a -> a

data Full a = Full a

instance V Int where
  fuller x = Full x
  unFuller (Full x) = x

instance V Char where
  fuller x = Full x
  unFuller (Full x) = x

data Empty = Empty
  deriving Show

instance V Empty where
  fuller x = Full x
  unFuller (Full x) = x

-- D a == da
data DList a da = DListMod Int da | DListCons a | Prepend [a] | Append [a] | FList [a]
instance V a => V [a] where
  type D [a] = DList a (D a)
  xs .+ DListMod i dx = take i xs ++ [x .+ dx] ++ drop (i+1) xs
    where x = xs !! i
  xs .+ DListCons x = x : xs
  xs .+ Prepend xs' = xs' ++ xs
  xs .+ Append xs' = xs ++ xs'
  _ .+ (FList xs') = xs'
  fuller xs = FList xs
  unFuller (FList xs) = xs

data DPair a da b db = DFst da | FDFst a | DSnd db | FDSnd b | FPair (a, b)
instance (V a, V b) => V (a, b) where
  type D (a, b) = DPair a (D a) b (D b)
  (a, b) .+ DFst da = (a .+ da, b)
  (a, b) .+ DSnd db = (a, b .+ db)
  (a, b) .+ FDFst a' = (a', b)
  (a, b) .+ FDSnd b' = (a, b')
  _ .+ (FPair p) = p
  fuller = FPair
  unFuller (FPair p) = p

data W = W { ints :: [Int], strings :: [String] }
  deriving Show
data DW = DWInts (DList Int (Full Int)) | FDWInts [Int] | DWStrings (DList String (DList Char (Full Char))) | FDWStrings [String] | FDW W
instance V W where
  type D W = DW
  w .+ DWInts di = w { ints = (ints w) .+ di }
  w .+ DWStrings ds = w { strings = (strings w) .+ ds }
  w .+ FDWInts ints = w { ints }
  w .+ FDWStrings strings = w { strings }
  _ .+ FDW w = w

-- This is not right -- this should be (F W a) as the first arg, but it demonstrates the right issue, for now
(<-+) :: V a => a -> D a -> a
(<-+) = (.+)

(<--) :: V a => a -> a -> a
x <-- y = x <-+ (fuller y)

tmiMain = do
  let w = W { ints = [1, 2, 3, 4], strings = ["asdf", "zxcv", "qwer"] }
  msp $ [Empty, Empty, Empty] .+ DListMod 1 (fuller Empty)
  msp $ [Empty, Empty, Empty] <-+ DListMod 1 (fuller Empty)
  msp $ [Empty, Empty, Empty] <-- [Empty, Empty]
  msp $ [1::Int, 2, 3] .+ Append [4, 5]
  msp $ (1::Int, "foo") .+ FDFst 2
  msp $ (1::Int, "foo") .+ DSnd (DListMod 1 (fuller 'q'))
  msp $ (1::Int, "foo") .+ DSnd (Prepend "s")
  msp $ (1::Int, "foo") .+ FDSnd "rrr"
  msp $ (1::Int, "foo") .+ FPair (2, "bar")
  msp $ [([1::Int, 2, 3], ("asdf", "zxcv"))] .+ DListMod 0 (DFst (DListMod 1 (fuller (20::Int))))
  msp $ [([1::Int, 2, 3], ("asdf", "zxcv"))] .+ DListMod 0 (DSnd (DFst (Append "g")))
  msp $ w .+ DWInts (DListMod 2 (fuller (30::Int)))
  msp $ w .+ DWStrings (DListMod 1 (fuller "lkjh"))
  msp $ w .+ DWStrings (DListMod 1 (Prepend "eee"))
  msp $ w .+ FDWInts [5, 6]
  msp $ w .+ FDWStrings ["ttt"]
  msp $ w .+ FDW (W { ints = [2, 3], strings = ["qwer"] })
  msp "hihi"
