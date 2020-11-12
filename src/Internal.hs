{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}
--{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Internal
( V
, aV
, r
, huh  -- just for returning some debug stuff
) where

import Data.Dynamic

import Util

-- Helpers for Dynamic stuff
dy :: Typeable a => a -> Dynamic
dy = toDyn
undy :: Typeable a => Dynamic -> a
undy dyn = case fromDynamic dyn of Just x -> x
                                   Nothing -> error msg
  where msg = "Can't convert " ++ (show (dynTypeRep dyn)) ++ " to a"

type D = [Dynamic]

-- Dump the types of the contained things
dInfo :: D -> String
dInfo ds = show (map dynTypeRep ds)

-- For lifting plain forward and reverse functions.  Functions are curried,
-- tuples turned into lists, and inputs in particular are turned into Vs.
--
-- Forward functions are curried, and then the input tuple and the output
-- singletons are turned into [Dynamic]s. (Output lists are typically length one
-- but don't have to be.) Any forward function becomes (D -> D).
--
-- Reverse functions are the same, except it takes the (old) input tuple and
-- new output tuple and returns the new input tuple, all in the form of
-- [Dynamic]. Any reverse function becomes (D -> D -> D).
--
-- The inputs are expected to be Vs. So if the original function takes an a,
-- the lifted function takes a (V a) and reads it with 'r'.
--
-- The reason to expect Vs (rather than reading them outside of this) is
-- because we can't read them without knowing the types, and types are hidden
-- by this lifting.
--
-- TODO: decrease the amount of code using Dynamic, ideally containing it
-- within a single function. It's easy to create bugs that get past the
-- typechecker.
liftFor_1_1 :: (Typeable a, Typeable b) => (a -> b) -> (D -> D)
liftFor_1_1 f [dyva] = [dy (f (r (undy dyva)))]
liftRev_1_1 :: (Typeable a, Typeable b) => (a -> b -> a) -> (D -> D -> D)
liftRev_1_1 rev [dyoa] [dyb] = [dy (rev (r (undy dyoa)) (undy dyb))]
liftFor_2_1 :: (Typeable a, Typeable b, Typeable c) => (a -> b -> c) -> (D -> D)
liftFor_2_1 f [dyx, dyy] = [dy (f (r (undy dyx)) (r (undy dyy)))]
liftRev_2_1 :: (Typeable a, Typeable b, Typeable c) => (a -> b -> c -> (a, b)) -> (D -> D -> D)
liftRev_2_1 rev [dyx, dyy] [dyz] = [dyx', dyy']
  where (x', y') = rev (r (undy dyx)) (r (undy dyy)) (undy dyz) 
        dyx' = dy x'
        dyy' = dy y'

-- An F is a plain Haskell lens.
data F a b = F { ffor :: a -> b, frev :: a -> b -> a }
data F2 a b c = F2 { ffor2 :: a -> b -> c, frev2 :: a -> b -> c -> (a, b) }

-- An S is a lifted F.
data S =
  S { for :: D -> D
    , rev :: D -> D -> D }

-- Lifters for Fs, composed from the forward/reverse lifters above.
-- TODO these two lifts are nearly the same
lift_1_1 :: (Typeable a, Typeable b) => F a b -> S
lift_1_1 (F {..}) = S { for, rev }
  where for = liftFor_1_1 ffor
        rev = liftRev_1_1 frev
lift_2_1 :: (Typeable a, Typeable b, Typeable c) => F2 a b c -> S
lift_2_1 (F2 {..}) = S { for, rev }
  where for = liftFor_2_1 ffor2
        rev = liftRev_2_1 frev2

-- An N is an S applied to arguments.
data N =
  N { n_s :: S
    , args :: D }

-- Apply an S to args to make an N.
applySD :: S -> D -> N
applySD s d = N { n_s = s, args = d }

-- Compute outputs from inputs
runNForwards :: N -> D
runNForwards (N {..}) = for n_s args -- (dynMap r args)

-- Compute inputs from outputs and old inputs
runNBackwards :: N -> D -> D
runNBackwards (N {..}) revArgs = rev n_s (for n_s args) revArgs

-- Bootstrapping the dag with some constant nodes.
-- TODO: use a lifter here?
konstS :: Typeable a => a -> S
konstS x = S {..}
  where for [] = [dy x]
        rev _ _ = error "Constant: no reverse"

konstN :: Typeable a => a -> N
konstN x = N {..}
  where n_s = konstS x
        args = []

konstV :: Typeable a => a -> V a
konstV x = V n 0
  where n = konstN x

-- A V can produce a value. What it produces the value from -- function and
-- arguments -- are hidden inside an S and a D, respectively, in turn held in
-- an N. The Int selects which of the N's outputs is the producer of this V's
-- value.
data V a = V N Int

-- Read a V by traversing the dag. Just enough to exercise all the plumbing.
r :: Typeable a => V a -> a
r (V n i) = undy $ (runNForwards n !! i)

-- Write to a V. This is barely any kind of useful evaluator. For one thing, it
-- doesn't consider whether any of the other outputs of the N are being written
-- to. It just reads the old outputs, replaces one of them, and writes them all
-- back. The result is a D containing the new inputs. Just enough to exercise
-- all the plumbing.
w :: Typeable a => V a -> a -> D
w (V n@(N {..}) i) x =
  let outputs :: [Dynamic]
      outputs = upd (runNForwards n) i (dy x)
   in rev n_s args outputs

-- Lifting the dynamic back into a static. This makes sure the types are right.
-- If the lifters don't have any bugs, then all the Dynamic conversions will
-- succeed, and outside code can't mess that up. This and konstV are the only
-- things that user code should have access to.
hoist2 :: (Typeable a, Typeable b, Typeable c) => F2 a b c -> (V a -> V b -> V c)
hoist2 f va vb = V (applySD (lift_2_1 f) [dy va, dy vb]) 0

-- reverse of (+)
split :: Integral a => a -> (a, a)
split x = (x', x'')
  where x' = x `div` 2
        x'' = x - x'
-- TODO why can't I make this Num?
pluss :: Integral a => a -> a -> a
pluss = (+)
revPlus :: Integral a => a -> a -> a -> (a, a)
revPlus _ _ x = split x
plusF :: Integral a => F2 a a a
plusF = F2 { ffor2 = pluss, frev2 = revPlus }

-- TODO If I annotate this, I get weird typeclass errors
-- aV :: (Show a, Typeable a, Integral a) => V a
aV = hoist2 plusF (konstV 40) (konstV 60)

aWrite :: String
aWrite = show $ ((case w aV (260::Int) of [dyx, dyy] -> (undy dyx, undy dyy)) :: (Int, Int))

--umm :: Show a => a
umm = r aV

huh :: String
huh = show (r aV, aWrite)

-- Some currying stuff

data Write = forall a. Show a => Write { showIt :: String }
data Q a b = Q (a -> ([Write], b))

infixr 0 +->
type a +-> b = Q a b

foo :: a +-> (b +-> c)
foo = undefined

data InOut a b = In (a -> b) | Out a b

-- doInOut (In f) x = f x
-- doInOut (Out a b) f
