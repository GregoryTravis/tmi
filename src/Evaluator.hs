{-# LANGUAGE RecordWildCards #-}

module Evaluator
( Simple(..)
) where

import Internal
import Util

data Simple = Simple

instance Evaluator Simple where
  -- readV :: Typeable a => e -> V a -> IO a
  readV evaluator (V n i) = do
    dyns <- runNForwards (Reader $ readV evaluator) n
    return $ undy $ (dyns !! i)

  -- writeV :: e -> [Write] -> e
  writeV = undefined
  -- w :: Typeable a => V a -> a -> Ds
  -- w (V n@(N {..}) i) x =
  --   let outputs :: [Dynamic]
  --       outputs = upd (runNForwards n) i (dy x)
  --    in rev n_s args outputs

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
