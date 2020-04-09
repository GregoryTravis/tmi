-- {-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}

module Tmi
( tmiMain ) where

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

-- The world, but also just a regular type
data W = W { anInt :: Int }
  deriving Show

-- _foo is an updater for a field foo
-- (foo) reads the field, while (_foo v) writes v to the field
_anInt :: W -> Int -> W
_anInt w i = w { anInt = i }

-- A change to W -- one option for each field of W.
-- The type name is 'D' + the full's name.
-- The field constructor names are 'D' + the field name (capitalized).
-- This states that a change to W is a change to one of the fields using a
-- value of a type that is the delta of that field's type.
--
-- This is the existential version, but not sure if I need it:
-- data DW d = (V d ~ Int) => DAnInt d
data DW d = DAnInt d

-- Now we define how we apply each type of DW -- it's just pulling wrappers
-- off
instance (V (DW d) ~ W, V d ~ Int, Delta d) => Delta (DW d) where
  type V (DW d) = W
  W { anInt = i } .+ DAnInt d = W { anInt = (i .+ d) }

-- A more concise way to write a delta to a field of a record
-- __anInt :: W -> DInt -> W -- too specific
_dAnInt :: (V d ~ Int, Delta d) => W -> d -> W
_dAnInt w di = w .+ DAnInt di

tmiMain = do
  let w :: W
      w = W { anInt = 10 }
  msp w
  msp $ _anInt w 11
  msp $ 34 .+ (DIntAdd 3)
  msp $ w .+ DAnInt (DIntAdd 5)
  msp $ _dAnInt w (DIntAdd 7)
  msp "hihi"
