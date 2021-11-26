{-# Language GADTs, NamedFieldPuns, TypeApplications #-}

module Veq where

import Data.Dynamic
import Type.Reflection

import Ty
import V

instance Eq (Bi f r) where
  Bi qf qr == Bi qf' qr' = qf == qf' && qr == qr'
  -- TODO: use dynamic stuff to do this?
  BiApp bi q == BiApp bi' q' = bi `biAppEq` bi' && q `qeq` q'
  _ == _ = False

instance Eq (V a) where
  VRoot == VRoot = True
  VNice x == VNice y = x == y
  VNamed name _ == VNamed name' _ = name == name'
  VBiSeal bi == VBiSeal bi' = bi == bi'
  _ == _ = False

biAppEq :: (Typeable a1, Typeable a2) => a1 -> a2 -> Bool
biAppEq bi bi' = biAppEqD (toDyn bi) (toDyn bi')
biAppEqD :: Dynamic -> Dynamic -> Bool
biAppEqD (Dynamic bit bi) (Dynamic bit' bi')
  | Just HRefl <- bit `eqTypeRep` bit'
  , App (App bitt x) y <- bit
  , Just HRefl <- bitt `eqTypeRep` (typeRep @Bi)
  = bi == bi'
biAppEqD _ _ = False

qeq q q' = qeqD (toDyn q) (toDyn q')
qeqD :: Dynamic -> Dynamic -> Bool
qeqD (Dynamic qt q) (Dynamic qt' q')
  | Just HRefl <- qt `eqTypeRep` qt'
  , App qtt x <- qt
  , Just HRefl <- qtt `eqTypeRep` (typeRep @V)
  = q == q'
qeqD _ _ = False
