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
    let dvs = map getDv listeners
    cache <- runAllNs dum writes
    let newW :: w
        newW = undy $ get cache VRoot
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
readV (Dum ws _) Root = return $ undy $ dy $ head ws

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

readyToRun :: [N] -> [N] -> N -> Bool
readyToRun _ alreadyTransferred n = all (`elem` alreadyTransferred) (revInputs n)
  where revInputs :: N -> [N]
        revInputs n = dvsN (args n)

-- Compute outputs from inputs
runNForwards :: Reader -> N -> IO Ds
runNForwards reader (N {..}) = for n_s reader args -- (dynMap r args)
