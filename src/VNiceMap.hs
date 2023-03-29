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

vallocAndSave :: V w NiceMap -> TMI w (V w NiceMap.Tag)
vallocAndSave vnm = do
  let (vtag, vnm') = vPairSplit $ valloc vnm
  vnm <-- vnm'
  vtag' <- Step $ Freeze vtag
  return vtag'

mkSlot :: forall a w. (Show w, Read a, Show a) => V w NiceMap -> a -> TMI w (V w a)
mkSlot vnm initialValue = do
  vtag <- vallocAndSave vnm
  let vs = vslot vnm vtag
  vs <--* initialValue
  return vs

valloc :: V w NiceMap -> V w (Tag, NiceMap)
valloc = lift1 $ nuni "alloc" alloc

vslot :: (Read a, Show a) => V w NiceMap -> V w Tag -> V w a
vslot = lift2 $ bi (VNamed "slot" slot) (VNamed "slot_" slot_)
