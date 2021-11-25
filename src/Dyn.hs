{-# Language GADTs, TypeApplications #-}

module Dyn where

import Data.Dynamic
import Data.Kind (Type)
import Type.Reflection

import Ty

qbiseal :: Dynamic -> Maybe Dynamic
qbiseal (Dynamic bit@(App (App bit0 at0) (App rt0 at1)) bi)
  | Just HRefl <- bit0 `eqTypeRep` (typeRep @Bi)
  , Just HRefl <- rt0 `eqTypeRep` (typeRep @R)
  , Just HRefl <- at0 `eqTypeRep` at1
  = Just (Dynamic (App (typeRep @Q) at0) (QBiSeal bi))

bi :: Dynamic -> Dynamic -> Maybe Dynamic
bi (Dynamic (App qt0 ft0) qf)
   (Dynamic (App qt1 rt0) qr)
  | Just HRefl <- qt0 `eqTypeRep` (typeRep @Q)
  , Just HRefl <- qt0 `eqTypeRep` qt1
  = Just (Dynamic (App (App (typeRep @Bi) ft0) rt0) (Bi qf qr))

bsbiapp :: Dynamic -> Dynamic -> Maybe Dynamic
bsbiapp (Dynamic (App (App bit0 (Fun at0 bt0))
                                (Fun at1 (Fun (App rt0 at2) ct0))) bi)
        (Dynamic (App qt0 at3) q)
  | Just HRefl <- qt0 `eqTypeRep` (typeRep @Q)
  , Just HRefl <- bit0 `eqTypeRep` (typeRep @Bi)
  , Just HRefl <- rt0 `eqTypeRep` (typeRep @R)
  , Just HRefl <- typeRep @Type `eqTypeRep` typeRepKind bt0
  , Just HRefl <- typeRep @Type `eqTypeRep` typeRepKind ct0
  , Just HRefl <- at0 `eqTypeRep` at1
  , Just HRefl <- at0 `eqTypeRep` at2
  , Just HRefl <- at0 `eqTypeRep` at3
  = Just (Dynamic (App (App bit0 bt0) ct0) (BiApp bi q))
