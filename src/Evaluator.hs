{-# LANGUAGE RecordWildCards #-}

module Evaluator
( Simple(..)
) where

--import Dyno
import Internal
import Util

-- DVs: listenees
data Simple = Simple DVs

dvKey :: DV -> Key
dvKey (DV key _) = key

vN :: V a -> N
vN (V n _) = n

instance Evaluator Simple where
  -- readV :: Typeable a => e -> V a -> IO a
  readV evaluator (V n i) = do
    dyns <- runNForwards (Reader $ readV evaluator) n
    return $ undy $ (dyns !! i)

  applyWrites (Simple listenees) writes = do
    msp $ map dvKey listenees
    msp "hi applied"

-- Starting from the listenees, return all Ns in reverse dependency order
-- TODO! this is just a haphazard traversal, we're not combining multiple
-- sequences at all
-- TODO remove dups
-- rToLNs :: Simple -> [N]
-- -- These are not ordered
-- rToLNs (Simple listenees) = concat $ map go (map vN listenees)
--   -- TODO Should be [N] -> [N] but how to combine them?
--   where go :: N -> [N]
--         go n = n : getPrecedents N
--         -- TODO these also are not ordered
--         getPrecedents (N {..}) = concat $ map go $ map vN args

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
