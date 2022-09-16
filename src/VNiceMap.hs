{-# Language ScopedTypeVariables #-}
{-# LANGUAGE MagicHash, BangPatterns #-}

module VNiceMap
( mkSlot ) where

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

-- A datatype that has the same layout as Word and so can be casted to it.
data Ptr' a = Ptr' a

-- Any is a type to which any type can be safely unsafeCoerced to.
aToWord# :: Any -> Word#
aToWord# a = let !mb = Ptr' a in case unsafeCoerce# mb :: Word of W# addr -> addr

unsafeAddr :: a -> Int
unsafeAddr a = I# (word2Int# (aToWord# (unsafeCoerce# a)))

slot :: (Read a, Show a) => NiceMap -> Tag -> a
slot nm tag =
  case lookup tag nm
    of Just a -> a
       Nothing -> error $ "VNiceMap slot: no such slot " ++ show tag ++ " " -- ++ nm
slot_ :: (Read a, Show a) => NiceMap -> R w NiceMap -> Tag -> R w Tag -> R w a
slot_ nm rnm tag _ = mkR r
  where r a = write rnm nm'
              where nm' = store tag a nm

mkSlot :: forall a w. (Show w, Read a, Show a) => String -> V w NiceMap -> a -> TMI w (V w a)
mkSlot tags vnm initialValue = do
  w <- Step $ Read VRoot
  Step $ Log $ show ("W", show w)
  let vtag :: V w Tag
      alloced = valloc vnm
      -- vtag = vfst alloced
      -- vnm' = vsnd alloced
      (vtag, vnm') = vPairSplit alloced
  vtag'' <- Step $ Ret $ k (rd w vtag)
  let vtag''' = vtag''
  let tag = rd w vtag
  Step $ Log $ show ("tag", show tag)
  let vtag' = k tag
  let vtag'addr = unsafeAddr vtag'
  let vtag'addr' = unsafeAddr vtag'
  let debug a = unsafePerformIO $ do
                  msp "mkSlot"
                  msp tags
                  msp vtag
                  msp tag
                  msp vtag'
                  msp vtag''
                  msp vtag'''
                  msp vtag'addr
                  msp vtag'addr'
                  msp w
                  return a
  let wrt = (VWrite vnm vnm')
  () <- Step $ WriteStep (debug wrt)
  let vtag'addr2 = unsafeAddr vtag'
  let vs = vslot vnm vtag'
  let debug2 a = unsafePerformIO $ do
                  msp "mkSlot2"
                  msp tags
                  msp vtag
                  msp tag
                  msp vtag'
                  msp vtag''
                  msp vtag'''
                  msp vtag'addr2
                  msp vs
                  msp w
                  return a
  vs <--* debug2 initialValue
  return $ vs

valloc :: V w NiceMap -> V w (Tag, NiceMap)
valloc = lift1 $ nuni "alloc" alloc

vslot :: (Read a, Show a) => V w NiceMap -> V w Tag -> V w a
vslot = lift2 $ bi (VNamed "slot" slot) (VNamed "slot_" slot_)
