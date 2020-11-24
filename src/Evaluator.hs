{-# LANGUAGE
  ExistentialQuantification
, MultiParamTypeClasses
, FlexibleInstances
, RankNTypes
, RecordWildCards
, ScopedTypeVariables #-}

module Evaluator
( Dum(..)
, mkHistory
, write
, mkListener
, addListener
) where

import Control.Monad (foldM)
import Data.List (nub, partition)
import qualified Data.Map as M
import Data.Typeable

--import Dyno
import Internal
import Util

-- ws are in reverse chronological order
data Dum w = Dum [w] [Listener]

instance History Dum w where
  mkHistory w = Dum [w] []
  addListener (Dum ws listeners) listener = Dum ws (listener : listeners)
  --addListener (Dum ws listeners) listener -
--write :: h w -> [Write] -> IO (h w)
  write dum@(Dum ws listeners) writes = do
    let dvs = map getDv listeners
    -- let ns = rToLNs dvs
    -- msp ns
    -- rToLNs and reader
    cache <- runAllNs dum writes
    let rootDV = VRoot $ dy $ mkRoot $ (1::Int)
    let newW :: w
        newW = undy $ get cache rootDV
    -- msp cache
    msp "hi applied"
    let newDum = case dum of Dum ws listeners -> Dum (newW:ws) listeners
    runListeners newDum
    return newDum

runListeners :: Nice w => Dum w -> IO ()
runListeners dum@(Dum ws listeners) = mapM_ runIt listeners
  where runIt :: Listener -> IO ()
        runIt (Listener {..}) = do
          msp "Running listener..."
          runReader reader
          msp "Ran listener"
        reader' :: forall a. Nice a => V a -> IO a 
        reader' = readV dum
        reader :: Reader
        reader = Reader { unReader = reader' }
--readV' :: (Nice w, Nice a) => Dum w -> V a -> IO a

-- Repeat until all Ns are added to the output list:
--   Find any N that is not in the list, but its inputs DVs have been reached, and all them to the lilst.
-- "Reaching" a DV means that it is the reverse output of an N that has been added to the list.
rToLNs :: DVs -> [N]
rToLNs dvs =
  let allNs = getAllNs dvs
   in transfer allNs readyToRun

-- Traverse and return all Ns, but not in dependency order.
getAllNs :: DVs -> [N]
getAllNs dvs = nub $ concat $ map (cascade srcsOf) ns
  where ns = dvsN dvs

mkListener :: Nice a => V a -> (a -> IO ()) -> Listener
mkListener v action = Listener {..}
  where getDv = dyv v
        runReader' :: Reader -> IO ()
        runReader' reader = do
          a <- unReader reader v
          action a
        runReader = runReader'

readV :: (Nice w, Nice a) => Dum w -> V a -> IO a
readV dum (V n i) = do
  dyns <- runNForwards (Reader $ readV dum) n
  return $ undy $ (dyns !! i)
readV (Dum ws _) (Root d) = return $ undy $ dy $ head ws

makeReaderWithCache :: Cache -> Reader -> Reader
makeReaderWithCache cache (Reader {..}) = Reader { unReader = unReader' }
  where unReader' va  = do
          eesp debug $ case getMaybe cache (dyv va) of
                         Just d -> return $ undy d
                         Nothing -> unReader va
          where debug = ("makeReaderWithCache va", toKey va, typeOf va)

runAllNs :: Nice w => Dum w -> [Write] -> IO Cache
runAllNs dum@(Dum _ listeners) writes =
  let cache = initFromWrites writes
      dvs = map getDv listeners 
      ns = rToLNs dvs
   in foldM (runAnN dum) cache ns

runAnN :: Nice w => Dum w -> Cache -> N -> IO Cache
runAnN dum cache n@(N {..}) = do
  msp ("runAnN", n)
  let r = rev n_s
      reader = Reader $ readV dum
      readerWithCache = makeReaderWithCache cache reader
  newInputs <- r reader readerWithCache args results
  -- TODO confusing the categories
  let writes = [Write dv d | (dv, d) <- zip args newInputs]
  return $ addWrites writes cache

newtype Cache = Cache (M.Map DV D)
  deriving Show

-- Insert a value. If the slot is empty, just add it. If it's
-- full, then confirm the new value is the same as the old value and do
-- nothing.
insert :: Cache -> DV -> D -> Cache
insert c@(Cache m) dv d =
  case M.lookup dv m of
    Just d' -> if d == d' then c else error $ show ("cache collision", d, d')
    Nothing -> Cache $ M.insert dv d m

-- has :: Cache -> DV -> Bool
-- has (Cache m) dv = M.member dv m

getMaybe :: Cache -> DV -> Maybe D
getMaybe (Cache m) dv = M.lookup dv m

get :: Cache -> DV -> D
get (Cache m) dv =
  case M.lookup dv m of
    Just d -> d
    Nothing -> error $ show ("cache get failure", dv)

empty :: Cache
empty = Cache M.empty

initFromWrites :: [Write] -> Cache
initFromWrites writes = addWrites writes empty

addWrites :: [Write] -> Cache -> Cache
addWrites writes cache = foldr addWrite cache writes
  where addWrite (Write dv d) c = insert c dv d

-- Transfer values from source list to destination list.
-- At each iteration, find those that are ready to transfer and transfer them.
-- readyToTransfer :: untransferred -> alreadyTransferred -> anElement -> shouldTransfer
-- NB: alElement should be one of the elements of untransferred, but we don't check that
transfer :: Show a => [a] -> ([a] -> [a] -> a -> Bool) -> [a]
transfer ins readyToTransfer = go ins []
  where go [] alreadyTransferred = alreadyTransferred
        go untransferred alreadyTransferred =
          let (someMoreToTransfer, stillUntransferred) = partition (readyToTransfer untransferred alreadyTransferred) untransferred
           in go stillUntransferred (someMoreToTransfer ++ alreadyTransferred)

readyToRun :: [N] -> [N] -> N -> Bool
readyToRun _ alreadyTransferred n = all (`elem` alreadyTransferred) (revInputs n)
  where revInputs :: N -> [N]
        revInputs n = dvsN (args n)

-- readyToRun :: [N] -> [N] -> [N]
-- readyToRun untransferred alreadyTransferred = filter ready untransferred
--   where ready :: N -> Bool
--         ready n = all (`elem` alreadyTransferred) (revInputs n)
--         revInputs :: N -> [N]
--         revInputs n = map dvN (args n)

-- Compute outputs from inputs
runNForwards :: Reader -> N -> IO Ds
runNForwards reader (N {..}) = for n_s reader args -- (dynMap r args)

---- Compute inputs from outputs and old inputs
--runNBackwards :: N -> Ds -> Ds
----runNBackwards (N {..}) revArgs = rev n_s (for n_s args) revArgs
--runNBackwards (N {..}) revArgs = rev n_s args revArgs


{-
-- This is to be an add-only and add-once map
data Gen

-- This should be in reverse chronological order, because the most common
-- operation is appending and looking up the most recent one.
-- Opaque type
data History w = History [Gen]

data Write = forall a. Write D a
read :: Int -> Key -> a
readHistory -> Key -> [a]
mkHistory :: w -> [D] -> History w
-- IO needed here?
update :: History w -> [Write] -> IO (History w)
-- Combine with update? So we can't forget?
runListeners :: History w -> IO ()
-}
