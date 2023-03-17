{-# Language GADTs, NamedFieldPuns, TypeApplications #-}

module Veq where

import Unsafe.Coerce
-- import Data.Dynamic
-- import Type.Reflection

import Ty
import V

instance Eq (Bi w f r) where
  Bi qf qr == Bi qf' qr' = qf == qf' && qr == qr'
  -- TODO: I *really* don't know if this is okay to do
  BiApp bi q == BiApp bi' q' = bi == (unsafeCoerce $ bi') && q == (unsafeCoerce q')
  _ == _ = False

instance Eq (V w a) where
  VRoot == VRoot = True
  VNice x == VNice y = x == y
  VNamed name _ == VNamed name' _ = name == name'
  VBiSeal bi == VBiSeal bi' = bi == bi'
  VFreeze n v == VFreeze n' v' = n == n && v == v'
  _ == _ = False

-- biAppEq :: (Typeable a1, Typeable a2) => a1 -> a2 -> Bool
-- biAppEq bi bi' = biAppEqD (toDyn bi) (toDyn bi')
-- biAppEqD :: Dynamic -> Dynamic -> Bool
-- biAppEqD (Dynamic bit bi) (Dynamic bit' bi')
--   | Just HRefl <- bit `eqTypeRep` bit'
--   , App (App (App bitt wt) xt) yt <- bit
--   , Just HRefl <- bitt `eqTypeRep` (typeRep @Bi)
--   = bi == bi'
-- biAppEqD _ _ = False

-- qeq q q' = qeqD (toDyn q) (toDyn q')
-- qeqD :: Dynamic -> Dynamic -> Bool
-- qeqD (Dynamic qt q) (Dynamic qt' q')
--   | Just HRefl <- qt `eqTypeRep` qt'
--   , App (App qtt wt) xt <- qt
--   , Just HRefl <- qtt `eqTypeRep` (typeRep @V)
--   = q == q'
-- qeqD _ _ = False
