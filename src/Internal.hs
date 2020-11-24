{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Internal
( Key
, Keyable(..)
, Nice
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
, Reader(..)
) where

--import Data.Dynamic
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Data.Typeable

import Dyno
import Hash
import Util

type Nice a = (Show a, Eq a, Typeable a)
-- This works too
-- class (Eq a, Typeable a) => Nice a
-- instance (Eq a, Typeable a) => Nice a

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

type D = Dyno
-- Dynamic (V a), plus key
data DV = DV Key N Dyno | VRoot
  deriving Show
type Ds = [D]
type DVs = [DV]

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
dyv :: (Typeable a) => V a -> DV
dyv va@(V n i) = DV (toKey va) n (dy va)
dyv Root = VRoot
undyv :: Typeable a => DV -> V a
undyv (DV _ _ dyn) = undy dyn
undyv VRoot = Root

-- Dump the types of the contained things
dInfo :: Ds -> String
dInfo ds = show (map getTypeRep ds)

-- Dump the types of the contained things
dvInfo :: DVs -> String
dvInfo ds = show (map f ds)
  where f (DV key _ dyn) = (key, getTypeRep dyn)

dvKey :: DV -> Key
dvKey (DV key _ _) = key
dvKey VRoot = rootKey

dvN :: DV -> Maybe N
dvN (DV _ n _) = Just n
dvN VRoot = Nothing

dvsN :: DVs -> [N]
dvsN dvs = catMaybes $ map dvN dvs

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
--
-- TODO use liftGeneric* here?
liftFor_0_1 :: Nice a => a -> (Reader -> DVs -> IO Ds)
liftFor_0_1 x _ [] = return [dy x]
-- Doesn't work: a vs (() -> a)
-- liftFor_0_1 = liftGeneric unPackage0 uncurry_0_1 package1

-- TODO this just prints out what was written.
liftRev_0_1 :: (Typeable a) => (a -> ()) -> (Reader -> Reader -> DVs -> DVs -> IO Ds)
liftRev_0_1 = undefined
-- This version was to allow writing to konst as if a konst was the world, in
-- an early test, but I think konst as a uni 0->1 is more consistent
-- liftRev_0_1 _ _ _ [] [dyNewOut] = do
--   msp ("liftRev_0_1 rev", dyNewOut)
--   return []

liftFor_1_1 :: (Nice a, Nice b) => (a -> b) -> (Reader -> DVs -> IO Ds)
liftFor_1_1 = liftGeneric unPackage1 uncurry_1_1 package1
-- -- orig impl
-- liftFor_1_1 f reader [dyva] = do -- [dy (f (r (undyv dyva)))]
--   let va = undyv dyva
--   a <- unReader reader va
--   return [dy (f a)]

liftRev_1_1 :: (Nice a, Nice b) => (a -> b -> a) -> (Reader -> Reader -> DVs -> DVs -> IO Ds)
liftRev_1_1 = liftGenericRev unPackage1 unPackage1 uncurryRev_1_1 package1
-- -- orig impl
-- liftRev_1_1 rev reader [dyova] [dyb] = do -- [dy (rev (r (undyv dyoa)) (undy dyb))]
--   let ova = undyv dyova
--   oa <- unReader reader ova
--   return [dy (rev oa (undy dyb))]

-- Unused, see liftFor_0_1
-- unPackage0 :: Reader -> DVs -> IO ()
-- unPackage0 _ _ = return ()

unPackage1 :: Nice a => Reader -> DVs -> IO a
unPackage1 reader [dyva] = do
  a <- unReader reader (undyv dyva)
  return a

unPackage2 :: (Nice a, Nice b) => Reader -> DVs -> IO (a, b)
unPackage2 reader [dyva, dyvb] = do
  a <- unReader reader (undyv dyva)
  b <- unReader reader (undyv dyvb)
  return (a, b)

unPackageOuts1 :: Nice a => Ds -> a
unPackageOuts1 [dya] = undy dya

unPackageOuts2 :: (Nice a, Nice b) => Ds -> (a, b)
unPackageOuts2 [dya, dyb] = (undy dya, undy dyb)

-- Unused, see liftFor_0_1
-- uncurry_0_1 :: a -> a
-- uncurry_0_1 = id

uncurry_1_1 :: (a -> b) -> (a -> b)
uncurry_1_1 = id

uncurry_2_1 :: (a -> b -> c) -> ((a, b) -> c)
uncurry_2_1 = uncurry

uncurry_1_2 :: (a -> (b, c)) -> (a -> (b, c))
uncurry_1_2 = id

uncurryRev_1_1 :: (a -> b -> a) -> (a -> b -> a)
uncurryRev_1_1 = id

uncurryRev_2_1 :: (a -> b -> c -> (a, b)) -> ((a, b) -> c -> (a, b))
uncurryRev_2_1 f (a, b) = f a b

uncurryRev_1_2 :: (a -> (b, c) -> a) -> (a -> (b, c) -> a)
uncurryRev_1_2 = id

package1 :: Nice a => a -> [D]
package1 x = [dy x]

package2 :: (Nice a, Nice b) => (a, b) -> [D]
package2 (a, b) = [dy a, dy b]

-- TODO getter -> unpackager etc
liftGeneric :: (Reader -> DVs -> IO tupleArgs) -> (cf -> (tupleArgs -> outs)) -> (outs -> [D]) -> cf -> Reader -> [DV] -> IO [D]
liftGeneric getter curryer packager f reader args =
  packager <$> curryer f <$> getter reader args
  -- let _ = (curryer f <$> getter reader args)
  --  in getter reader args >>= return . curryer f >>= (return . packager)

-- TODO consistent names, use Ds, Dvs everywhere
liftGenericRev :: (Reader -> DVs -> IO tupleArgs) -> (Reader -> DVs -> IO tupleNewOuts) -> (cr -> (tupleArgs -> tupleNewOuts -> newIns)) -> (newIns -> [D]) -> cr ->
                  Reader -> Reader -> [DV] -> [DV] -> IO [D]
liftGenericRev getter outsGetter curryer packager r reader readerWithCache args newouts =
  packager <$> (curryer r <$> getter reader args <*> (outsGetter readerWithCache newouts))
-- -- orig impl -- keep this around, I have no idea how the magic above works
-- liftGenericRev getter outsGetter curryer packager r reader args newouts = do
--   oldIns <- getter reader args
--   let newOuts = outsGetter newouts
--       newIns = curryer r oldIns newOuts
--   return $ packager newIns

liftFor_2_1 :: (Nice a, Nice b, Nice c) => (a -> b -> c) -> (Reader -> DVs -> IO Ds)
liftFor_2_1 = liftGeneric unPackage2 uncurry_2_1 package1
-- -- orig impl
-- liftFor_2_1 f reader args@[dyvx, dyvy] = do -- [dy (f (r (undyv dyx)) (r (undyv dyy)))]
--   let vx = undyv dyvx
--       vy = undyv dyvy
--   x <- unReader reader vx
--   y <- unReader reader vy
--   return [dy (f x y)]

liftRev_2_1 :: (Nice a, Nice b, Nice c) => (a -> b -> c -> (a, b)) -> (Reader -> Reader -> DVs -> DVs -> IO Ds)
liftRev_2_1 = liftGenericRev unPackage2 unPackage1 uncurryRev_2_1 package2
-- -- works
-- liftRev_2_1 rev reader args newouts = do -- [dyx', dyy']
--   oldins <- unPackage2 reader args
--   let newins = uncurryRev_2_1 rev oldins (unPackageOuts1 newouts)
--   return $ package2 newins
-- -- orig impl
-- liftRev_2_1 rev reader [dyovx, dyovy] [dyz] = do -- [dyx', dyy']
--   let ovx = undyv dyovx
--       ovy = undyv dyovy
--   ox <- unReader reader ovx
--   oy <- unReader reader ovy
--   let (x, y) = rev ox oy (undy dyz)
--   return [dy x, dy y]

-- TODO this seems like maybe an inconsistent form for inputs & outsputs generally.
liftFor_1_2 :: (Nice a, Nice b, Nice c) => (a -> (b, c)) -> (Reader -> DVs -> IO Ds)
liftFor_1_2 = liftGeneric unPackage1 uncurry_1_2 package2
-- -- orig impl
-- liftFor_1_2 f reader [dyva] = do
--   let va = undyv dyva
--   a <- unReader reader va
--   let (b, c) = f a
--   return [dy b, dy c]

liftRev_1_2 :: (Nice a, Nice b, Nice c) => (a -> (b, c) -> a) -> (Reader -> Reader -> DVs -> DVs -> IO Ds)
liftRev_1_2 = liftGenericRev unPackage1 unPackage2 uncurryRev_1_2 package1
-- -- orig impl
-- liftRev_1_2 rev reader [dyova] [dyb, dyc] = do
--   let ova = undyv dyova
--       b = undy dyb
--       c = undy dyc
--   oa <- unReader reader ova
--   let a = rev oa (b, c)
--   return [dy a]

-- An F is a plain Haskell lens.
data F0 a = F0 { name0 :: String, ffor0 :: a, frev0 :: a -> () }
data F a b = F { name :: String, ffor :: a -> b, frev :: a -> b -> a }
data F2 a b c = F2 { name2 :: String, ffor2 :: a -> b -> c, frev2 :: a -> b -> c -> (a, b) }
-- TODO F2 -> F_2_2 etc
data F_1_2 a b c = F_1_2 { name_1_2 :: String, ffor_1_2 :: a -> (b, c), frev_1_2 :: a -> (b, c) -> a }

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

-- TODO should be renamed wrap_2_1 when F2 becomes F_2_1, etc
noisy1 :: (Nice a, Nice b) => F a b -> F a b
noisy1 (F {..}) = F name ffor' frev'
  where ffor' = wrap_for_1_1 (\a b -> eesp (name, "for", a, "->", b) b) ffor
        frev' = wrap_rev_1_1 (\a b a' -> eesp (name, "rev", a, b, "->", a') a') frev
noisy2 :: (Nice a, Nice b, Nice c) => F2 a b c -> F2 a b c
noisy2 (F2 {..}) = F2 name2 ffor2' frev2'
  where ffor2' = wrap_for_2_1 (\a b c -> eesp (name2, "for", a, b, "->", c) c) ffor2
        frev2' = wrap_rev_2_1 (\a b c (a', b') -> eesp (name2, "rev", a, b, c, "->", (a', b')) (a', b')) frev2

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
    --, outputsBuilder :: N -> DVs
    , for :: Reader -> DVs -> IO Ds
    , rev :: Reader -> Reader -> DVs -> DVs -> IO Ds }

-- This is correct only because we ensure (by hand) that every S has a unique
-- name.
instance Eq S where
  s == s' = names s == names s'

-- Lifters for Fs, composed from the forward/reverse lifters above.
-- TODO these two lifts are nearly the same
lift_0_1 :: Nice a => F0 a -> S
lift_0_1 (F0 {..}) = S {..}
  where for = liftFor_0_1 ffor0
        rev = liftRev_0_1 frev0
        names = name0
        --outputsBuilder n = let (dv, v) = mkOutputDVAndV n 0 in undefined
        --outputsBuilder n = [dyv $ mkOutput n 0]
        --outputsBuilder n = [dyv $ ((V n 0) :: Typeable a => V a)]
lift_1_1 :: (Nice a, Nice b) => F a b -> S
lift_1_1 (F {..}) = S {..}
  where for = liftFor_1_1 ffor
        rev = liftRev_1_1 frev
        names = name
        -- outputsBuilder n = let (dv, v) = mkOutputDVAndV n 0 in undefined
lift_2_1 :: (Nice a, Nice b, Nice c) => F2 a b c -> S
lift_2_1 (F2 {..}) = S {..}
  where for = liftFor_2_1 ffor2
        rev = liftRev_2_1 frev2
        names = name2
lift_1_2 :: (Nice a, Nice b, Nice c) => F_1_2 a b c -> S
lift_1_2 (F_1_2 {..}) = S {..}
  where for = liftFor_1_2 ffor_1_2
        rev = liftRev_1_2 frev_1_2
        names = name_1_2

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
           getKey VRoot = rootKey

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
  where n = applySD (lift_1_2 f) [dyv va] [dyv vb, dyv vc]
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
  readV :: (Nice w, Nice a) => h w -> V a -> IO a

data Listener = forall a. Nice a => Listener
  { v :: V a
  , action :: a -> IO ()
  , runReader :: Reader -> IO ()
  , getDv :: DV }

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
