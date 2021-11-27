module Lift
( lift1
, lift2 ) where

import Data.Dynamic

import Ty

lift1 :: (Typeable a, Typeable b, Typeable w) => Bi w (a -> b) (a -> R w a -> R w b) -> V w a -> V w b
lift1 bi qa = VBiSeal (BiApp bi qa)

lift2 :: (Typeable a, Typeable b, Typeable c, Typeable w) => Bi w (a -> b -> c) (a -> R w a -> b -> R w b -> R w c) -> V w a -> V w b -> V w c
lift2 bi qa qb = VBiSeal (BiApp (BiApp bi qa) qb)
