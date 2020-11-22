{-# LANGUAGE RecordWildCards #-}

module Evaluator
( Simple(..)
) where

import Data.List (nub, partition)
import qualified Data.Map as M

--import Dyno
import Internal
import Util

-- DVs: listenees
data Simple = Simple DVs

instance Evaluator Simple where
  -- readV :: Typeable a => e -> V a -> IO a
  readV evaluator (V n i) = do
    dyns <- runNForwards (Reader $ readV evaluator) n
    return $ undy $ (dyns !! i)

--applyWrites :: e -> [Write] -> IO ()
  applyWrites evaluator@(Simple listenees) writes = do
    let ns = rToLNs evaluator
    msp ns
    msp "hi applied"

-- Initialize a cache with (DV, D) writes.
-- Repeat until all Ns are added to the output list:
--   Find any N that is not in the list, but its inputs DVs have been reached, and all them to the lilst.
-- "Reaching" a DV means that it is the reverse output of an N that has been added to the list.
rToLNs :: Simple -> [N]
rToLNs evaluator =
  let allNs = getAllNs evaluator
   in transfer allNs readyToRun

-- Traverse and return all Ns, but not in dependency order.
getAllNs :: Simple -> [N]
getAllNs (Simple dvs) = nub $ concat $ map (cascade srcsOf) ns
  where ns = map dvN dvs

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
        revInputs n = map dvN (args n)

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
