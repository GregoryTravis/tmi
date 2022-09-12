module Lib where

import Lift
import Ty
import Util
import V

(!!.) :: V w [a] -> V w Int -> V w a
(!!.) = lift2 $ nuni "!!." (!!)

vfst :: V w (a, b) -> V w a
vfst = lift1 $ bi (VNamed "fst" fst) (VNamed "fst_" fst_)
fst_ :: (a, b) -> R w (a, b) -> R w a
fst_ (_, b) rpr = mkR r
  where r a = write rpr (a, b)

vsnd :: V w (a, b) -> V w b
vsnd = lift1 $ bi (VNamed "snd" snd) (VNamed "snd_" snd_)
snd_ :: (a, b) -> R w (a, b) -> R w b
snd_ (a, _) rpr = mkR r
  where r b = write rpr (a, b)

-- TODO not good, not good at all
-- TODO could be made generic for single-ctor data types
vPairSplit :: V w (a, b) -> (V w a, V w b)
vPairSplit vpr = (vfst vpr, vsnd vpr)

-- untested
-- vcons :: V w a -> V w [a] -> V w [a]
-- vcons = lift2 $ nuni "vcons" (:)
