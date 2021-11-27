{-# Language GADTs, TypeApplications #-}

module Dyn where

import Data.Dynamic
import Data.Kind (Type)
import Type.Reflection

import Ty

-- VBiSeal :: Bi w a (R w a) -> V w a
qbiseal :: Dynamic -> Maybe Dynamic
qbiseal (Dynamic bit@(App (App (App bit0 w0) at0) (App (App rt0 w1) at1)) bi)
  | Just HRefl <- bit0 `eqTypeRep` (typeRep @Bi)
  , Just HRefl <- rt0 `eqTypeRep` (typeRep @R)
  , Just HRefl <- at0 `eqTypeRep` at1
  , Just HRefl <- w0 `eqTypeRep` w1
  = Just (Dynamic (App (App (typeRep @V) w0) at0) (VBiSeal bi))

-- Bi :: (Typeable f, Typeable r) => V w f -> V w r -> Bi w f r
bi :: Dynamic -> Dynamic -> Maybe Dynamic
bi (Dynamic (App (App qt0 w0) ft0) qf)
   (Dynamic (App (App qt1 w1) rt0) qr)
  | Just HRefl <- qt0 `eqTypeRep` (typeRep @V)
  , Just HRefl <- qt0 `eqTypeRep` qt1
  , Just HRefl <- w0 `eqTypeRep` w1
  = withTypeable ft0 $ withTypeable rt0 $ withTypeable w0 $
      Just (Dynamic (App (App (App (typeRep @Bi) w0) ft0) rt0) (Bi qf qr))

--BiApp :: (Typeable a, Typeable b, Typeable c) => Bi w (a -> b) (a -> R a -> c) -> V w a -> Bi w b c
bsbiapp :: Dynamic -> Dynamic -> Maybe Dynamic
bsbiapp (Dynamic (App (App (App bit0 w0)
                                    (Fun at0 bt0))
                                    (Fun at1 (Fun (App (App rt0 w1) at2) ct0))) bi)
        (Dynamic (App (App qt0 w2) at3) q)
  | Just HRefl <- qt0 `eqTypeRep` (typeRep @V)
  , Just HRefl <- bit0 `eqTypeRep` (typeRep @Bi)
  , Just HRefl <- rt0 `eqTypeRep` (typeRep @R)
  , Just HRefl <- typeRep @Type `eqTypeRep` typeRepKind bt0
  , Just HRefl <- typeRep @Type `eqTypeRep` typeRepKind ct0
  , Just HRefl <- w0 `eqTypeRep` w1
  , Just HRefl <- w0 `eqTypeRep` w2
  , Just HRefl <- at0 `eqTypeRep` at1
  , Just HRefl <- at0 `eqTypeRep` at2
  , Just HRefl <- at0 `eqTypeRep` at3
  = withTypeable at0 $ withTypeable bt0 $ withTypeable ct0 $ withTypeable w0 $
      Just (Dynamic (App (App (App bit0 w0) bt0) ct0) (BiApp bi q))
