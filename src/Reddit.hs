{-# Language AllowAmbiguousTypes, ExistentialQuantification, GADTs, RankNTypes, ScopedTypeVariables, PartialTypeSignatures, TypeApplications, KindSignatures #-}

module Reddit (redditMain) where

import Data.Dynamic
import Data.Kind (Type)
import Data.Proxy
import Data.Maybe (fromJust)
import Type.Reflection
import Unsafe.Coerce (unsafeCoerce)

import Util

data Q a where
  Q :: a -> Q a
  QF :: (a -> b) -> Q (a -> b)
  QApp :: Q (a -> b) -> Q a -> Q b

--  :: Q (a->b) -> Q a      -> Maybe (Q b)
app :: Dynamic  -> Dynamic -> Maybe Dynamic
app (Dynamic (App q (Fun ta tr)) qf) (Dynamic (App q' ta') qx)
  | Just HRefl <- q `eqTypeRep` q'
  , Just HRefl <- q `eqTypeRep` (typeRep @Q)
  , Just HRefl <- ta `eqTypeRep` ta'
  , Just HRefl <- typeRep @Type `eqTypeRep` typeRepKind tr
  = Just (Dynamic (App q tr) (QApp qf qx))

dynApply' :: Dynamic -> Dynamic -> Maybe Dynamic
dynApply' (Dynamic (Fun ta tr) f) (Dynamic ta' x)
  | Just HRefl <- ta `eqTypeRep` ta'
  , Just HRefl <- typeRep @Type `eqTypeRep` typeRepKind tr
  = Just (Dynamic tr (f x))
dynApply' _ _
  = Nothing

root = Q (10 :: Int)
vaa = QF (+ (1::Int))

redditMain = do
  let Just foo = app (toDyn vaa) (toDyn root)
  msp foo
  let qi = fromJust $ fromDynamic foo :: Q Int
  msp $ case qi of QApp (QF f) (Q x) -> f x
