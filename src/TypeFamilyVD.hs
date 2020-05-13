{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- The unfortunately named TypeFamilyVD uses a type family that maps a single
-- delta to any type. It still seems to require type annotation in int
-- literals.

module TypeFamilyVD
( typeFamilyVDMain ) where

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
  fuller = FDW
  unFuller (FDW w) = w

data VW a b = VW { as :: [a], bs :: [b] }
 deriving Show
data DVW a da b db = DVWAs (DList a da) | FDVWAs [a] | DVWBs (DList b db) | FDVWBs [b] | FDVW (VW a b)
instance (V a, V b) => V (VW a b) where
  type D (VW a b) = DVW a (D a) b (D b)
  vw .+ DVWAs das = vw { as = (as vw) .+ das }
  vw .+ DVWBs dbs = vw { bs = (bs vw) .+ dbs }
  vw .+ FDVWAs as = vw { as }
  vw .+ FDVWBs bs = vw { bs }
  _ .+ FDVW vw = vw
  fuller = FDVW
  unFuller (FDVW vw) = vw

data Something a b = This a | That a [b]
  deriving Show
data DSomething a da b db = DThis da | FDThis a | DThat0 da | DThat1 (DList b db) | FDThat0 a | FDThat1 [b] | FDSomething (Something a b)
instance (V a, V b) => V (Something a b) where
  type D (Something a b) = DSomething a (D a) b (D b)
  This a .+ DThis da = This (a .+ da)
  fuller = FDSomething
  unFuller (FDSomething x) = x

data EDelta a = forall da. (D a ~ da) => EDelta da | FEDelta a
data Compound a = Compound [EDelta a] | FCompound a

data ForDelta a = ForDelta a
  deriving Show
instance V a => V (ForDelta a) where
  type D (ForDelta a) = EDelta a
  ForDelta x .+ EDelta dx = ForDelta (x .+ dx)
  fuller (ForDelta x) = FEDelta x
  unFuller (FEDelta x) = ForDelta x

data ForCompound a = ForCompound a
  deriving Show
unForCompound (ForCompound x) = x
instance V a => V (ForCompound a) where
  type D (ForCompound a) = Compound a
  ForCompound x .+ Compound (EDelta da:das) = ForCompound (x .+ da) .+ Compound das
  ForCompound x .+ Compound [] = ForCompound x
  fuller (ForCompound x) = FCompound x
  unFuller (FCompound x) = ForCompound x

(.++) :: V a => a -> [EDelta a] -> a
a .++ das = unForCompound $ ForCompound a .+ Compound das

-- This is not right -- this should be (F W a) as the first arg, but it demonstrates the right issue, for now
(<-+) :: V a => a -> D a -> a
(<-+) = (.+)

(<--) :: V a => a -> a -> a
x <-- y = x <-+ (fuller y)

typeFamilyVDMain = do
  let w = W { ints = [1, 2, 3, 4], strings = ["asdf", "zxcv", "qwer"] }

  msp $ ForCompound [1::Int, 2, 3] .+ Compound [EDelta $ DListMod 1 (fuller (10::Int)), EDelta $ Prepend [50, 60]]

  -- msp $ [Empty, Empty, Empty] .+ DListMod 1 (fuller Empty)
  -- msp $ [Empty, Empty, Empty] <-+ DListMod 1 (fuller Empty)
  -- msp $ [Empty, Empty, Empty] <-- [Empty, Empty]
  -- msp $ [1::Int, 2, 3] .+ Append [4, 5]
  -- msp $ (1::Int, "foo") .+ FDFst 2
  -- msp $ (1::Int, "foo") .+ DSnd (DListMod 1 (fuller 'q'))
  -- msp $ (1::Int, "foo") .+ DSnd (Prepend "s")
  -- msp $ (1::Int, "foo") .+ FDSnd "rrr"
  -- msp $ (1::Int, "foo") .+ FPair (2, "bar")
  -- msp $ [([1::Int, 2, 3], ("asdf", "zxcv"))] .+ DListMod 0 (DFst (DListMod 1 (fuller (20::Int))))
  -- msp $ [([1::Int, 2, 3], ("asdf", "zxcv"))] .+ DListMod 0 (DSnd (DFst (Append "g")))
  -- msp $ w .+ DWInts (DListMod 2 (fuller (30::Int)))
  -- msp $ w .+ DWStrings (DListMod 1 (fuller "lkjh"))
  -- msp $ w .+ DWStrings (DListMod 1 (Prepend "eee"))
  -- msp $ w .+ FDWInts [5, 6]
  -- msp $ w .+ FDWStrings ["ttt"]
  -- msp $ w .+ FDW (W { ints = [2, 3], strings = ["qwer"] })
  -- let vw :: VW (Int, String) Char
  --     vw = VW { as = [(1, "one")], bs = "sdfgz" }
  -- msp $ vw .+ DVWAs (DListMod 0 (DFst (fuller (10::Int))))
  -- msp $ vw .+ DVWBs (DListMod 0 (fuller 'S'))
  -- msp $ vw .+ FDVWAs [(100, "hundred")]
  -- msp $ vw .+ FDVWBs "pppp"
  -- let th :: Something [Int] (Char, String)
  --     th = This [1::Int, 2, 3]
  -- msp $ th .+ DThis (DListMod 2 (fuller (30::Int)))
  -- msp $ This @[Int] @(Char, String) [1::Int, 2, 3] .+ DThis (DListMod 2 (fuller (31::Int)))
  msp "hihi"
