{-# Language ScopedTypeVariables #-}
{-# LANGUAGE MagicHash, BangPatterns #-}

module VNiceMap
( mkSlot
, unTagV ) where

import GHC.Exts
import Prelude hiding (lookup)
import System.IO.Unsafe

import Lib
import Lift
import NiceMap
import Propagate
import TMI
import Ty hiding (store)
import V
import Util

-- -- A datatype that has the same layout as Word and so can be casted to it.
-- data Ptr' a = Ptr' a

-- -- Any is a type to which any type can be safely unsafeCoerced to.
-- aToWord# :: Any -> Word#
-- aToWord# a = let !mb = Ptr' a in case unsafeCoerce# mb :: Word of W# addr -> addr

-- unsafeAddr :: a -> Int
-- unsafeAddr a = I# (word2Int# (aToWord# (unsafeCoerce# a)))

unTagV = ulift1 "unTag" unTag

slot :: (Read a, Show a) => NiceMap -> Tag -> a
slot nm tag =
  case lookup tag nm
    of Just a -> a
       Nothing -> error $ "VNiceMap slot: no such slot " ++ show tag ++ " " -- ++ nm
slot_ :: (Read a, Show a) => NiceMap -> R w NiceMap -> Tag -> R w Tag -> R w a
slot_ nm rnm tag _ = mkR r
  where r a = write rnm nm'
              where nm' = store tag a nm

mkSlot :: forall a w. (Show w, Read a, Show a) => V w NiceMap -> a -> TMI w (V w a)
mkSlot vnm initialValue = do
  w <- Step $ Read VRoot
  Step $ Log $ show ("W", show w)
  let vtag :: V w Tag
      alloced = valloc vnm
      -- vtag = vfst alloced
      -- vnm' = vsnd alloced
      (vtag, vnm') = vPairSplit alloced
  tagn <- Step $ Read (unTagV vtag)
  let vtag' = VFreeze 23 (k (Tag tagn))
  let debug a = unsafePerformIO $ do
                  -- msp "mkSlot"
                  -- msp tagn
                  -- msp vtag'
                  return a
  () <- debug $ vnm <-- vnm'
  let vs = vslot vnm vtag'
  let debug2 a = unsafePerformIO $ do
                  -- msp "mkSlot2"
                  -- msp tagn
                  -- msp vtag'
                  return a
  vs <--* debug2 initialValue
  return $ vs

valloc :: V w NiceMap -> V w (Tag, NiceMap)
valloc = lift1 $ nuni "alloc" alloc

vslot :: (Read a, Show a) => V w NiceMap -> V w Tag -> V w a
vslot = lift2 $ bi (VNamed "slot" slot) (VNamed "slot_" slot_)
