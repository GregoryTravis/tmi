{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
--{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module FreshStart
( freshStartMain ) where

import Util

-- A type d is a delta for type (V d) if you can use it to change a (V d) into
-- another (V d).
-- So V is a type-level function taking a delta and returning its full.
-- We say (V d ~ Int) to mean that d is some delta for Int.
-- We generally use 'd' for delta types like 'a' for regular types.
class Delta d where
  type V d
  (.+) :: V d -> d -> V d

-- Any type might have multiple deltas, so you give them names.
-- We could call this DInt if we think there will be only one.
data DIntAdd = DIntAdd Int

instance Delta DIntAdd where
  type V DIntAdd = Int
  i .+ (DIntAdd di) = i + di

data DDoubleAdd = DDoubleAdd Double
instance Delta DDoubleAdd where
  type V DDoubleAdd = Double
  d .+ (DDoubleAdd dd) = d + dd

-- The world, but also just a regular type
data W = W { anInt :: Int
           , anotherInt :: Int
           , aDouble :: Double }
  deriving Show

-- _foo is an updater for a field foo
-- (foo) reads the field, while (_foo v) writes v to the field
_anInt :: W -> Int -> W
_anInt w i = w { anInt = i }
_anotherInt :: W -> Int -> W
_anotherInt w i = w { anotherInt = i }
_aDouble :: W -> Double -> W
_aDouble w d = w { aDouble = d }

-- A change to W -- one option for each field of W.
-- The type name is 'D' + the full's name.
-- The field constructor names are 'D' + the field name (capitalized).
-- This states that a change to W is a change to one of the fields using a
-- value of a type that is the delta of that field's type.
--
-- This is the existential version, but not sure if I need it:
-- data DW d = (V d ~ Int) => DAnInt d
data DW d q = (V d ~ Int, V q ~ Double) => DAnInt d | DAnotherInt d | DADouble q

-- Now we define how we apply each type of DW -- it's just pulling wrappers
-- off
instance (V (DW d q) ~ W, V d ~ Int, Delta d, V q ~ Double, Delta q) => Delta (DW d q) where
  type V (DW d q) = W
  w@(W { anInt = i }) .+ DAnInt d = w { anInt = (i .+ d) }
  w@(W { anotherInt = i }) .+ DAnotherInt d = w { anotherInt = (i .+ d) }
  w@(W { aDouble = d }) .+ DADouble dd = w { aDouble = (d .+ dd) }

-- A more concise way to write a delta to a field of a record
-- __anInt :: W -> DInt -> W -- too specific
--_dAnInt :: (V d ~ Int, Delta d) => W -> d -> W
--_dAnInt w di = w .+ DAnInt di
--_dAnInt w di = w .+ (DAnInt di :: (V (DW d q) ~ W, V d ~ Int, Delta d, V q ~ Double, Delta q) => DW d q)
-- _dAnotherInt :: (V d ~ Int, Delta d) => W -> d -> W
-- _dAnotherInt w di = w .+ DAnotherInt di

-- data Foo a b = Foo (Either a b)
--   deriving Show

-- class C a where
--   bar :: a -> (a, a)

-- class P a where
--   yah :: a -> a

-- instance P Int where
--   yah = id

-- instance (P a, P b) => C (Foo a b) where
--   bar x@(Foo (Left y)) = (x, x)

-- example = do
--   putStrLn $ show $ bar $ Foo (Left 1)

freshStartMain = do
  --example
  let w :: W
      w = W { anInt = 10
            , anotherInt = 100
            , aDouble = 3.3 }
  msp w
  msp $ 34 .+ (DIntAdd 3)
  msp $ w .+ ((DAnInt (DIntAdd 5)) :: DW DIntAdd DDoubleAdd)
  msp $ w .+ ((DAnotherInt (DIntAdd 6)) :: DW DIntAdd DDoubleAdd)
  msp $ w .+ ((DADouble (DDoubleAdd 0.2)) :: DW DIntAdd DDoubleAdd)
  -- msp $ _anInt w 11
  -- msp $ _dAnInt w (DIntAdd 7)
  -- msp $ _dAnotherInt w (DIntAdd 60)
  msp "hihi"
