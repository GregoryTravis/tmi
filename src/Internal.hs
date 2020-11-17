{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Internal
( V(..)
, F(..)
, F2(..)
, F_1_2(..)
, S(..)
, N(..)
, D
, DV
, Ds
, DVs
, hoist_1_1
, hoist_2_1
, hoist_1_2
, konstV
, undy -- Just for debugging in the absence of an evaluator
, Evaluator(..)
, Reader(..)
) where

import Data.Dynamic

import Hash
import Util

data Key = Key String
  deriving (Show, Eq, Ord)

class Keyable a where
  toKey :: a -> Key

instance Keyable String where
  toKey = Key . id
instance Keyable Int where
  toKey = Key . show

-- compositeKey :: Keyable a => [a] -> Key
-- compositeKey keyables = Key $ hash $ concat $ map (\(Key s) -> s) $ map toKey keyables
compositeKey :: [Key] -> Key
compositeKey keys = Key $ hash $ concat $ map (\(Key s) -> s) keys

type D = Dynamic
-- Dynamic (V a), plus key
data DV = DV Key Dynamic
type Ds = [D]
type DVs = [DV]

-- Helpers for Dynamic stuff
dy :: Typeable a => a -> D
dy = toDyn
undy :: Typeable a => D -> a
undy dyn = case fromDynamic dyn of Just x -> x
                                   Nothing -> error msg
  where msg = "Can't convert " ++ (show (dynTypeRep dyn)) ++ " to a"
dyv :: Typeable a => V a -> DV
dyv va = DV (toKey va) (dy va)
undyv :: Typeable a => DV -> V a
undyv (DV _ dyn) = undy dyn

-- Dump the types of the contained things
dInfo :: Ds -> String
dInfo ds = show (map dynTypeRep ds)

-- Dump the types of the contained things
dvInfo :: DVs -> String
dvInfo ds = show (map f ds)
  where f (DV key dyn) = (key, dynTypeRep dyn)

-- For lifting plain forward and reverse functions.  Functions are curried,
-- tuples turned into lists, and inputs in particular are turned into Vs.
--
-- Forward functions are curried, and then the input tuple and the output
-- singletons are turned into [Dynamic]s. (Output lists are typically length one
-- but don't have to be.) Any forward function becomes (Ds -> Ds).
--
-- Reverse functions are the same, except it takes the (old) input tuple and
-- new output tuple and returns the new input tuple, all in the form of
-- [Dynamic]. Any reverse function becomes (Ds -> Ds -> Ds).
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
liftFor_0_1 :: Typeable a => a -> (Reader -> DVs -> IO Ds)
liftFor_0_1 x _ [] = return [dy x]

liftRev_0_1 :: (Typeable a) => (a -> ()) -> (Reader -> DVs -> Ds -> IO Ds)
liftRev_0_1 _ = undefined

liftFor_1_1 :: (Typeable a, Typeable b) => (a -> b) -> (Reader -> DVs -> IO Ds)
liftFor_1_1 f reader [dyva] = do -- [dy (f (r (undyv dyva)))]
  let va = undyv dyva
  a <- unReader reader va
  return [dy (f a)]

liftRev_1_1 :: (Typeable a, Typeable b) => (a -> b -> a) -> (Reader -> DVs -> Ds -> IO Ds)
liftRev_1_1 rev reader [dyova] [dyb] = do -- [dy (rev (r (undyv dyoa)) (undy dyb))]
  let ova = undyv dyova
  oa <- unReader reader ova
  return [dy (rev oa (undy dyb))]

liftFor_2_1 :: (Typeable a, Typeable b, Typeable c) => (a -> b -> c) -> (Reader -> DVs -> IO Ds)
liftFor_2_1 f reader [dyvx, dyvy] = do -- [dy (f (r (undyv dyx)) (r (undyv dyy)))]
  let vx = undyv dyvx
      vy = undyv dyvy
  x <- unReader reader vx
  y <- unReader reader vy
  return [dy (f x y)]
  -- Alternate implementation
  -- (:[]) <$> dy <$> (f <$> dyread dyvx <*> dyread dyvy)
  -- where dyread :: forall a. Typeable a => DV -> IO a
  --       dyread = unReader reader . undyv

liftRev_2_1 :: (Typeable a, Typeable b, Typeable c) => (a -> b -> c -> (a, b)) -> (Reader -> DVs -> Ds -> IO Ds)
liftRev_2_1 rev reader [dyovx, dyovy] [dyz] = do -- [dyx', dyy']
  let ovx = undyv dyovx
      ovy = undyv dyovy
  ox <- unReader reader ovx
  oy <- unReader reader ovy
  let (x, y) = rev ox oy (undy dyz)
  return [dy x, dy y]

-- TODO this seems like maybe an inconsistent form for inputs & outsputs generally.
liftFor_1_2 :: (Typeable a, Typeable b, Typeable c) => (a -> (b, c)) -> (Reader -> DVs -> IO Ds)
liftFor_1_2 f reader [dyva] = do
  let va = undyv dyva
  a <- unReader reader va
  let (b, c) = f a
  return [dy b, dy c]

liftRev_1_2 :: (Typeable a, Typeable b, Typeable c) => (a -> (b, c) -> a) -> (Reader -> DVs -> Ds -> IO Ds)
liftRev_1_2 rev reader [dyova] [dyb, dyc] = do
  let ova = undyv dyova
      b = undy dyb
      c = undy dyc
  oa <- unReader reader ova
  let a = rev oa (b, c)
  return [dy a]

-- An F is a plain Haskell lens.
data F0 a = F0 { name0 :: String, ffor0 :: a, frev0 :: a -> () }
data F a b = F { name :: String, ffor :: a -> b, frev :: a -> b -> a }
data F2 a b c = F2 { name2 :: String, ffor2 :: a -> b -> c, frev2 :: a -> b -> c -> (a, b) }
-- TODO F2 -> F_2_2 etc
data F_1_2 a b c = F_1_2 { name_1_2 :: String, ffor_1_2 :: a -> (b, c), frev_1_2 :: a -> (b, c) -> a }

instance Keyable (F0 a) where
  toKey (F0 {..}) = Key name0
instance Keyable (F a b) where
  toKey (F {..}) = Key name
instance Keyable (F2 a b c) where
  toKey (F2 {..}) = Key name2

-- An S is a lifted F.
data S =
  S { names :: String
    , for :: Reader -> DVs -> IO Ds
    , rev :: Reader -> DVs -> Ds -> IO Ds }

-- Lifters for Fs, composed from the forward/reverse lifters above.
-- TODO these two lifts are nearly the same
lift_0_1 :: Typeable a => F0 a -> S
lift_0_1 (F0 {..}) = S {..}
  where for = liftFor_0_1 ffor0
        rev = liftRev_0_1 frev0
        names = name0
lift_1_1 :: (Typeable a, Typeable b) => F a b -> S
lift_1_1 (F {..}) = S {..}
  where for = liftFor_1_1 ffor
        rev = liftRev_1_1 frev
        names = name
lift_2_1 :: (Typeable a, Typeable b, Typeable c) => F2 a b c -> S
lift_2_1 (F2 {..}) = S {..}
  where for = liftFor_2_1 ffor2
        rev = liftRev_2_1 frev2
        names = name2
lift_1_2 :: (Typeable a, Typeable b, Typeable c) => F_1_2 a b c -> S
lift_1_2 (F_1_2 {..}) = S {..}
  where for = liftFor_1_2 ffor_1_2
        rev = liftRev_1_2 frev_1_2
        names = name_1_2

instance Keyable S where
  toKey (S {..}) = Key names

-- An N is an S applied to arguments.
data N =
  N { n_s :: S
    , args :: DVs }

instance Keyable N where
   toKey (N {..}) = compositeKey ((toKey $ names n_s) : map getKey args)
     where getKey (DV key _) = key

-- Apply an S to args to make an N.
applySD :: S -> DVs -> N
applySD s d = N { n_s = s, args = d }

-- A V can produce a value. What it produces the value from -- function and
-- arguments -- are hidden inside an S and a Ds, respectively, in turn held in
-- an N. The Int selects which of the N's outputs is the producer of this V's
-- value.
data V a = V N Int

instance Keyable (V a) where
   toKey (V n i) = compositeKey [toKey n, toKey i]

-- Lifting the dynamic back into a static. This makes sure the types are right.
-- If the lifters don't have any bugs, then all the Dynamic conversions will
-- succeed, and outside code can't mess that up. This and konstV are the only
-- things that user code should have access to.
hoist_0_1 :: Typeable a => F0 a -> V a
hoist_0_1 f = V (applySD (lift_0_1 f) []) 0
hoist_1_1 :: (Typeable a, Typeable b) => F a b -> (V a -> V b)
hoist_1_1 f va = V (applySD (lift_1_1 f) [dyv va]) 0
hoist_2_1 :: (Typeable a, Typeable b, Typeable c) => F2 a b c -> (V a -> V b -> V c)
hoist_2_1 f va vb = V (applySD (lift_2_1 f) [dyv va, dyv vb]) 0
hoist_1_2 :: (Typeable a, Typeable b, Typeable c) => F_1_2 a b c -> (V a -> (V b, V c))
hoist_1_2 f va = (vb, vc)
  where n = applySD (lift_1_2 f) [dyv va]
        vb = V n 0
        vc = V n 1

konstV :: (Show a, Typeable a) => a -> V a
konstV x = hoist_0_1 $ F0 { name0, ffor0 = x, frev0 = undefined }
  where name0 = hash x

data Write = Write (DV, D)

class Evaluator e where
  readV :: Typeable a => e -> V a -> IO a
  writeV :: e -> [Write] -> e

--newtype Id = Id { unId :: forall a. a -> a }
newtype Reader = Reader { unReader :: forall a. Typeable a => V a -> IO a }

-- -- Some currying stuff

-- data Write = forall a. Show a => Write { showIt :: String }
-- data Q a b = Q (a -> ([Write], b))

-- infixr 0 +->
-- type a +-> b = Q a b

-- foo :: a +-> (b +-> c)
-- foo = undefined

-- data InOut a b = In (a -> b) | Out a b

-- -- doInOut (In f) x = f x
-- -- doInOut (Out a b) f
