{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
, F0(..)
, F(..)
, F2(..)
, F_1_2(..)
, S(..)
, N(..)
, D
, DV(..)
, Ds
, DVs
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
, TMI
) where

import Control.Monad.State (StateT)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Data.Typeable

import Dyno
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

data Write = Write DV D

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

-- Monad!
type TMI h w a = (Nice w, History h w) => StateT (h w) IO a
