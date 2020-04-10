{-# LANGUAGE AllowAmbiguousTypes #-}
--{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Example
( exampleMain ) where

import Util

-- This is an example I posted to reddit:
-- https://www.reddit.com/r/haskell/comments/fsgqd6/monthly_hask_anything_april_2020/fn05vra/
--
-- It's just a stripped down version of my "starting over" attempt over in
-- Tmi.hs.
--
-- Later on in the file is another try using multiparm type classes &
-- fundeps; it seems to have the same problem.

{-

I'm struggling to get something to compile. The full code is below, and I'm not really sure if I could simplify it any further, but it boils down to this:

-- I've got some complicated constraints needed for this implementation of
-- Delta and its method (.+)
instance (V (DW d q) ~ W, V d ~ Int, Delta d, V q ~ Double, Delta q)
         => Delta (DW d q) where
  type V (DW d q) = W
  (.+) = ...

-- Each constructor in this type takes only one of the two arguments
data DW d q = DAnInt d | DAnotherInt d | DADouble q

-- When I try to use a value of type (DW d q) as a Delta here, I can prove
-- that the constraints on 'd' are satisfied, but I cannot prove anything
-- about 'q' since 'q' isn't used in this particular value.
_dAnInt :: (V d ~ Int, Delta d) => W -> d -> W
_dAnInt w di = w .+ DAnInt di
--                  ^^^^^^^^^
-- ---------------------^

I want to say "Don't worry, this value (DAnInt di) meets all the constraints needed for (Delta (DW d q)), don't worry about 'q' here because it isn't involved anyway". I've tried adding explicit annotations but I get lots of things about not being able to unify (d0 ~ d).

-}

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

-- A change to W -- one option for each field of W.
-- The type name is 'D' + the full's name.
-- The field constructor names are 'D' + the field name (capitalized).
-- This states that a change to W is a change to one of the fields using a
-- value of a type that is the delta of that field's type.
--
-- This is the existential version, but not sure if I need it:
-- data DW d = (V d ~ Int) => DAnInt d
data DW d q = DAnInt d | DAnotherInt d | DADouble q

-- Now we define how we apply each type of DW -- it's just pulling wrappers
-- off
instance (V (DW d q) ~ W, V d ~ Int, Delta d, V q ~ Double, Delta q) => Delta (DW d q) where
  type V (DW d q) = W
  w@(W { anInt = i }) .+ DAnInt d = w { anInt = (i .+ d) }
  w@(W { anotherInt = i }) .+ DAnotherInt d = w { anotherInt = (i .+ d) }
  w@(W { aDouble = d }) .+ DADouble dd = w { aDouble = (d .+ dd) }

-- _dAnInt :: (V d ~ Int, Delta d) => W -> d -> W
-- _dAnInt w di = w .+ DAnInt di

exampleMain = do
  msp "hihi"

{-

-- add float
-- another int
-- remove DW type vars?

class Delta a da | da -> a where 
  (.+) :: a -> da -> a

data DIntAdd = DIntAdd Int
instance Delta Int DIntAdd where
  i .+ (DIntAdd di) = i + di

data DDoubleAdd = DDoubleAdd Double
instance Delta Double DDoubleAdd where
  d .+ (DDoubleAdd dd) = d + dd

data W = W { anInt :: Int
           , aDouble :: Double }
  deriving Show

-- Errors: same old thing, it can't prove anything about the type that
-- isn't used in each particular construtor. If I add annotations I get the
-- usual "I can't unify the thing I want with the thing you said is what I
-- wait", usually of the form "can't unify d0 with d".
--
-- data DW di dd = DAnInt di | DADouble dd
-- instance (Delta Int di, Delta Double dd) => Delta W (DW di dd) where
--   w@(W { anInt = i }) .+ (DAnInt di) = w { anInt = i .+ di }

-- Same
--data DW = forall di dd . (Delta Int di, Delta Double dd) =>  DADouble dd | DAnInt di
data (Delta Int di, Delta Double dd) => DW di dd = DAnInt di | DADouble dd 
instance (Delta Int di, Delta Double dd) => Delta W (DW di dd) where
  w@(W { anInt = i }) .+ (DAnInt di) = w { anInt = i .+ di }

exampleMain = do
  let w = W { anInt = 10
            , aDouble = 3.3 }
  msp w
  msp $ w .+ (DAnInt (DIntAdd 3) :: (Delta Double dd) => DW DIntAdd dd)
  msp "hihi"
-}
