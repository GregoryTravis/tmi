module Lift
( lift1
, lift2 ) where

import Data.Dynamic

import Ty

lift1 :: (Typeable a, Typeable b) => Bi (a -> b) (a -> R a -> R b) -> V a -> V b
lift1 bi qa = VBiSeal (BiApp bi qa)

lift2 :: (Typeable a, Typeable b, Typeable c) => Bi (a -> b -> c) (a -> R a -> b -> R b -> R c) -> V a -> V b -> V c
lift2 bi qa qb = VBiSeal (BiApp (BiApp bi qa) qb)
