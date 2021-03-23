{-# LANGUAGE
  ExistentialQuantification
, MultiParamTypeClasses
, FlexibleInstances
, RankNTypes
, RecordWildCards
, ScopedTypeVariables #-}

module History
( Dum(..)
, mkHistory
, write
, addListener
, readV
) where

import Control.Monad (foldM)
import Data.List (nub)
import qualified Data.Map as M
import Data.Typeable

import Internal
import Util

-- ws are in reverse chronological order
data Dum w = Dum [w] [Listener]

instance History Dum w where
  mkHistory w = Dum [w] []

  addListener (Dum ws listeners) listener = Dum ws (listener : listeners)

  write dum@(Dum ws listeners) writes = do
    --let dvs = map getDv listeners
    cache <- runAllNs dum writes
    -- TODO this undefined here is wrong wrong wrong
    let newW :: w
        newW = undy $ get cache DRoot
    let newDum = case dum of Dum ws listeners -> Dum (newW:ws) listeners
    runListeners newDum
    return newDum

--readV :: (Nice w, Nice a) => Dum w -> V a -> IO a
  readV dum (V n i) = do
    dyns <- runNForwards dum (Reader $ readV dum) n
    return $ undy $ (dyns !! i)

  -- TODO use some kind of cast here
  readV (Dum ws _) Root = return $ undy $ dy $ head ws

  -- runListeners :: Nice w => Dum w -> IO ()
  runListeners dum@(Dum ws listeners) = mapM_ runIt (reverse listeners)
    where runIt :: Listener -> IO ()
          runIt (Listener {..}) = do
            msp "Running listener..."
            runReader reader
            msp "Ran listener"
          reader = Reader { unReader = readV dum }

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

-- makeReaderWithCache :: Cache -> Reader -> Reader
-- makeReaderWithCache cache (Reader {..}) = Reader { unReader = unReader' }
--   where unReader' va  = do
--           case getMaybe cache (dyv va) of
--             Just d -> return $ undy d
--             Nothing -> unReader va

runAllNs :: Nice w => Dum w -> [Write] -> IO Cache
runAllNs dum@(Dum _ listeners) writes =
  let cache = initFromWrites writes
      --dvs = map getDv listeners 
      dvs = map wdv writes
        where wdv (Write dv _) = dv
      ns = rToLNs dvs
   in foldM (runNBackwards dum) cache ns

readDV :: (Nice w) => Dum w -> Reader -> DV -> IO D
readDV dum reader (DV _ _ dReader) = dReader reader
readDV (Dum ws _) reader DRoot = return $ dy $ head ws

readDVWithCache :: (Nice w) => Cache -> Dum w -> Reader -> DV -> IO D
readDVWithCache cache dum reader dv = do
  case getMaybe cache dv of
    Just d -> return d
    Nothing -> readDV dum reader dv

-- Compute outputs from inputs
runNForwards :: Nice w => Dum w -> Reader -> N -> IO Ds
runNForwards dum reader (N {..}) = do
  argVals <- mapM (readDV dum reader) args
  return $ for n_s argVals

-- Run the N backwards, reading reverse inputs from the cache and writing the
-- reverse outputs to the cache.
runNBackwards :: Nice w => Dum w -> Cache -> N -> IO Cache
runNBackwards dum cache n@(N {..}) = do
  let r = rev n_s
      reader = Reader $ readV dum
      --readerWithCache = makeReaderWithCache cache reader
  argsVals <- mapM (readDV dum reader) args :: IO Ds
  resultsVals <- mapM (readDVWithCache cache dum reader) results :: IO Ds
  let newInputs = rev n_s argsVals resultsVals
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
    Just d' -> if d == d' then c else error $ show ("cache collision", dv, "new", d, "cache", d')
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

readyToRun :: [N] -> [N] -> N -> Bool
readyToRun _ alreadyTransferred n = all (`elem` alreadyTransferred) (revInputs n)
  where revInputs :: N -> [N]
        revInputs n = dvsN (args n)
