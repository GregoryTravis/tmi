{-# Language ScopedTypeVariables #-}

module VNiceMap
( mkSlot ) where

import Prelude hiding (lookup)

import Lib
import Lift
import NiceMap
import TMI
import Ty hiding (store)
import V
import Util

slot :: (Read a, Show a) => NiceMap -> Tag -> a
slot nm tag =
  case lookup tag nm
    of Just a -> a
       Nothing -> error $ "VNiceMap slot: no such slot " ++ show tag ++ " " -- ++ nm
slot_ :: (Read a, Show a) => NiceMap -> R w NiceMap -> Tag -> R w Tag -> R w a
slot_ nm rnm tag _ = mkR r
  where r a = write rnm nm'
              where nm' = store tag a nm

mkSlot :: forall a w. (Read a, Show a) => V w NiceMap -> a -> TMI w (V w a)
mkSlot vnm initialValue = do
  let vtag :: V w Tag
      alloced = valloc vnm
      -- vtag = vfst alloced
      -- vnm' = vsnd alloced
      (vtag, vnm') = vPairSplit alloced
  tag <- Step $ Read vtag
  let vtag' = k tag
  () <- Step $ WriteStep (VWrite vnm vnm')
  let vs = vslot vnm vtag'
  vs <--* initialValue
  return vs

valloc :: V w NiceMap -> V w (Tag, NiceMap)
valloc = lift1 $ nuni "alloc" alloc

vslot :: (Read a, Show a) => V w NiceMap -> V w Tag -> V w a
vslot = lift2 $ bi (VNamed "slot" slot) (VNamed "slot_" slot_)
