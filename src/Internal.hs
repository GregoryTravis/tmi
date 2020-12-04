{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Internal
( Nice
, V(..)
, mkRoot
, F(..)
, F2(..)
, F_1_2(..)
, S(..)
, N(..)
, D
, DV(..)
, Ds
, DVs
, hoist_1_1
, hoist_2_1
, hoist_1_2
, konstV
, vN
, dy
, undy -- Just for debugging in the absence of an evaluator
, dyv
, dvKey
, dvN
, dvsN
, srcsOf
, Write(..)
, History(..)
, Listener(..)
, mkListener
, Reader(..)
, TMI
) where

import Control.Monad.State (StateT)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Data.Typeable

import Dyno
import Hash
import Key
import Util

type Nice a = (Show a, Eq a, Typeable a)
-- This works too
-- class (Eq a, Typeable a) => Nice a
-- instance (Eq a, Typeable a) => Nice a

type D = Dyno
-- Dynamic (V a), plus key
data DV = forall a. DV Key N (Reader -> IO D) | DRoot
type Ds = [D]
type DVs = [DV]

instance Show DV where
  show (DV key n _) = "(DV " ++ show key ++ " " ++ show n ++ ")"

-- We ensure that keys are unique by using a hash and ensuring that all names
-- for things (F, S) are unique.
-- TODO use deriving via?
instance Eq DV where
  dv == dv' = dvKey dv == dvKey dv'

-- We ensure that keys are unique by using a hash and ensuring that all names
-- for things (F, S) are unique.
-- TODO use deriving via?
instance Ord DV where
  -- TODO use compareBy or something
  compare dv dv' = compare (dvKey dv) (dvKey dv')

blah :: (Typeable a, Typeable b) => (a -> b) -> a -> b
blah f x = eesp ("blah", debug) $ f x
  where debug = (typeOf x, typeOf $ f x)

-- Helpers for Dynamic stuff
dy :: (Nice a) => a -> D
dy = mkDyno
undy :: Nice a => D -> a
undy dyn =
  let a' :: a
      a' = undefined
      --a'Type = typeOf a'
      result = case getit dyn of
                 Just x -> x
                 Nothing -> error $ msg result
   in result
  where msg desired = "Can't convert " ++ (show (getTypeRep dyn)) ++ " to a " ++ (show (typeOf desired))
        huh = blah (+10) (100::Int)
dyv :: Nice a => V a -> DV
dyv v@(V n i) = DV key n dReader
  where key = toKey v
        dReader reader = do
          a <- unReader reader v
          return $ dy a
dyv Root = DRoot
--   where dReader reader = do
--           a <- unReader reader (Root :: forall a. Show a => V a)
--           return $ dy a
-- undyv :: Typeable a => DV -> V a
-- undyv (DV _ _ dyn) = undy dyn
-- undyv DRoot = Root

-- Dump the types of the contained things
dInfo :: Ds -> String
dInfo ds = show (map getTypeRep ds)

-- Dump the types of the contained things
dvInfo :: DVs -> String
dvInfo ds = show (map f ds)
  where f (DV key _ dyn) = key -- (key, getTypeRep dyn)

dvKey :: DV -> Key
dvKey (DV key _ _) = key
dvKey DRoot = rootKey

dvN :: DV -> Maybe N
dvN (DV _ n _) = Just n
dvN DRoot = Nothing

dvsN :: DVs -> [N]
dvsN dvs = catMaybes $ map dvN dvs

-- An F is a plain Haskell lens.
data F0 a = F0 { name0 :: String, ffor0 :: a, frev0 :: a -> () }
data F a b = F { name :: String, ffor :: a -> b, frev :: a -> b -> a }
data F2 a b c = F2 { name2 :: String, ffor2 :: a -> b -> c, frev2 :: a -> b -> c -> (a, b) }
-- TODO F2 -> F_2_2 etc
data F_1_2 a b c = F_1_2 { name_1_2 :: String, ffor_1_2 :: a -> (b, c), frev_1_2 :: a -> (b, c) -> a }

-- A wrapper takes the input and output, does something with those, and returns the output.
-- TODO shouldn't have to take care to return the output
wrap1 :: (a -> b -> b) -> (a -> b) -> (a -> b)
wrap1 wrapper f a = let b = f a in wrapper a b
wrap2 :: (a -> b -> c -> c) -> (a -> b -> c) -> (a -> b -> c)
wrap2 wrapper f a b = let c = f a b in wrapper a b c
wrap3 :: (a -> b -> c -> d -> d) -> (a -> b -> c -> d) -> (a -> b -> c -> d)
wrap3 wrapper r a b c = let d = r a b c in wrapper a b c d

wrap_for_1_1 = wrap1
wrap_rev_1_1 = wrap2
wrap_for_2_1 = wrap2
wrap_rev_2_1 = wrap3
wrap_for_1_2 = wrap1
wrap_rev_1_2 = wrap2

-- TODO should be renamed wrap_2_1 when F2 becomes F_2_1, etc
noisy1 :: (Nice a, Nice b) => F a b -> F a b
noisy1 (F {..}) = F name ffor' frev'
  where ffor' = wrap_for_1_1 (\a b -> eesp (name, "for", a, "->", b) b) ffor
        frev' = wrap_rev_1_1 (\a b a' -> eesp (name, "rev", a, b, "->", a') a') frev
noisy2 :: (Nice a, Nice b, Nice c) => F2 a b c -> F2 a b c
noisy2 (F2 {..}) = F2 name2 ffor2' frev2'
  where ffor2' = wrap_for_2_1 (\a b c -> eesp (name2, "for", a, b, "->", c) c) ffor2
        frev2' = wrap_rev_2_1 (\a b c (a', b') -> eesp (name2, "rev", a, b, c, "->", (a', b')) (a', b')) frev2
noisy_1_2 (F_1_2 {..}) = F_1_2 name_1_2 ffor_1_2' frev_1_2'
  where ffor_1_2' = wrap_for_1_2 (\a bc -> eesp (name_1_2, "for", a, "->", bc) bc) ffor_1_2
        frev_1_2' = wrap_rev_1_2 (\a bc bc' -> eesp (name_1_2, "rev", a, bc, "->", bc') bc') frev_1_2

instance Keyable (F0 a) where
  toKey (F0 {..}) = Key name0
instance Keyable (F a b) where
  toKey (F {..}) = Key name
instance Keyable (F2 a b c) where
  toKey (F2 {..}) = Key name2

-- An S is a lifted F.
-- TODO rename 'names', it sounds plural but it really is just the name of an S
data S =
  S { names :: String
    , for :: Ds -> Ds
    , rev :: Ds -> Ds -> Ds }

-- This is correct only because we ensure (by hand) that every S has a unique
-- name.
instance Eq S where
  s == s' = names s == names s'

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
lift_0_1 :: Nice a => F0 a -> S
lift_0_1 (F0 {..}) = S {..}
  where for [] = [dy $ ffor0]
        rev = undefined
        names = name0

lift_1_1 :: (Nice a, Nice b) => F a b -> S
lift_1_1 (F {..}) = S {..}
  where for = dys1 . ffor . undys1
        rev [doin0] [dnout0] = [dy nin0]
          where nin0 = frev (undy doin0) (undy dnout0)
        names = name

lift_2_1 :: (Nice a, Nice b, Nice c) => F2 a b c -> S
lift_2_1 (F2 {..}) = S {..}
  where for = dys1 . uncurry ffor2 . undys2
        rev [doin0, doin1] [dnout0] = [dy nin0, dy nin1]
          where (nin0, nin1) = frev2 (undy doin0) (undy doin1) (undy dnout0)
        names = name2

lift_1_2 :: (Nice a, Nice b, Nice c) => F_1_2 a b c -> S
lift_1_2 (F_1_2 {..}) = S {..}
  --where for dins = dys2 $ ffor_1_2 (undys1 dins)
  where for = dys2 . ffor_1_2 . undys1
        rev doins dnouts = dys1 $ frev_1_2 (undys1 doins) (undys2 dnouts)
        names = name_1_2

-- Convenicenes for converting between typed and untyped value bundles
dys1 :: (Nice a) => a -> [D]
dys1 a = [dy a]
undys1 :: (Nice a) => [D] -> a
undys1 [da] = undy da
dys2 :: (Nice a, Nice b) => (a, b) -> [D]
dys2 (a, b) = [dy a, dy b]
undys2 :: (Nice a, Nice b) => [D] -> (a, b)
undys2 [da, db] = (undy da, undy db)

instance Keyable S where
  toKey (S {..}) = Key names

-- An N is an S applied to arguments.
data N =
  N { n_s :: S
    , args :: DVs
    , results :: DVs }
  deriving Eq

-- mkOutput :: Typeable a => N -> Int -> V a
-- mkOutput n i = V n i
-- mkOutputDVAndV :: Typeable a => N -> Int -> (DV, V a)
-- mkOutputDVAndV n i =
--   let v = mkOutput n i
--    in (dyv v, v)
-- -- mkOutputDV :: Typeable a => N -> Int -> DV
-- -- mkOutputDV n i = fst $ mkOutputDVAndV n i

instance Show N where
  show (N {..}) = "(N " ++ names n_s ++ " " ++ showArgs ++ ")"
    where showArgs = intercalate " " (map show $ map dvKey args)

instance Keyable N where
   toKey (N {..}) = compositeKey ((toKey $ names n_s) : map getKey args)
     where getKey (DV key _ _) = key
           getKey DRoot = rootKey

-- DAG traversal stuff
srcsOf :: N -> [N]
srcsOf n = dvsN (args n)

-- -- We can reconstruct the output DVs from the # of outputs.
-- -- They are (V n i) for each output i
-- outputsOf :: N -> DVs
-- outputsOf n = map mk [0..numOutputs (n_s n)]
--   where mk i = dyv $ V n i

-- Apply an S to args to make an N.
applySD :: S -> DVs -> DVs -> N
applySD s dvs outputDVs = N { n_s = s, args = dvs, results = outputDVs }

-- A V can produce a value. What it produces the value from -- function and
-- arguments -- are hidden inside an S and a Ds, respectively, in turn held in
-- an N. The Int selects which of the N's outputs is the producer of this V's
-- value.
data V a = V N Int | Root
mkRoot :: Nice a => a -> V a
mkRoot _ = Root

vN :: V a -> N
vN (V n _) = n

instance Keyable (V a) where
   toKey (V n i) = compositeKey [toKey n, toKey i]
   toKey Root = rootKey

rootKey :: Key
rootKey = Key ""

instance Show (V a) where
  show v = show $ toKey v

instance Eq (V a) where
  v == v' = toKey v == toKey v'

-- Lifting the dynamic back into a static. This makes sure the types are right.
-- If the lifters don't have any bugs, then all the Dynamic conversions will
-- succeed, and outside code can't mess that up. This and konstV are the only
-- things that user code should have access to.
--
-- I had to tie the knot between the N and its outputs (the argument of applySD
-- is a list of dynamicized copies of the returned nodes; the returned nodes
-- point to the N returned by applySD). I couldn't find any other place where
-- it would let me call dyv. It works here because we're return the Vs, so we
-- know the type of (V n i), so dyv will take it. I would rather it be done
-- otherwise but this is the only way I could make it work.
hoist_0_1 :: Nice a => F0 a -> V a
hoist_0_1 f =
  let n = applySD (lift_0_1 f) [] [dyv v]
      v = V n 0
   in v

hoist_1_1 :: (Nice a, Nice b) => F a b -> (V a -> V b)
hoist_1_1 f va =
  let n = (applySD (lift_1_1 (noisy1 f)) [dyv va] [dyv v])
      v = V n 0
   in v

hoist_2_1 :: (Nice a, Nice b, Nice c) => F2 a b c -> (V a -> V b -> V c)
hoist_2_1 f va vb =
  let n = applySD (lift_2_1 (noisy2 f)) [dyv va, dyv vb] [dyv v]
      v = V n 0
   in v

hoist_1_2 :: (Nice a, Nice b, Nice c) => F_1_2 a b c -> (V a -> (V b, V c))
hoist_1_2 f va = (vb, vc)
  where n = applySD (lift_1_2 (noisy_1_2 f)) [dyv va] [dyv vb, dyv vc]
        vb = V n 0
        vc = V n 1

konstV :: (Show a, Nice a) => a -> V a
konstV x = hoist_0_1 $ F0 { name0, ffor0 = x, frev0 = undefined }
  where name0 = hash x

data Write = Write DV D

--newtype Id = Id { unId :: forall a. a -> a }
newtype Reader = Reader { unReader :: forall a. Nice a => V a -> IO a }

class History h w where
  mkHistory :: w -> h w
  addListener :: h w -> Listener -> h w
  write :: Nice w => h w -> [Write] -> IO (h w)
  -- TODO debug onlyl
  readV :: (Show a, Nice w, Nice a) => h w -> V a -> IO a
  runListeners :: Nice w => h w -> IO ()

data Listener = forall a. Nice a => Listener
  { v :: V a
  , action :: a -> IO ()
  , runReader :: Reader -> IO ()
  , getDv :: DV }

mkListener :: Nice a => V a -> (a -> IO ()) -> Listener
mkListener v action = Listener {..}
  where getDv = dyv v
        runReader :: Reader -> IO ()
        runReader reader = do
          a <- unReader reader v
          action a

-- Monad!
type TMI h w a = (Nice w, History h w) => StateT (h w) IO a

