module Lift
( lift1
, lift2
, liftV ) where

import Data.Dynamic

import Ty
import V

lift1 :: Bi w (a -> b) (a -> R w a -> R w b) -> V w a -> V w b
lift1 bi qa = VBiSeal (BiApp bi qa)

lift2 :: Bi w (a -> b -> c) (a -> R w a -> b -> R w b -> R w c) -> V w a -> V w b -> V w c
lift2 bi qa qb = VBiSeal (BiApp (BiApp bi qa) qb)

liftV :: (a -> b -> a) -> (a -> R w a -> R w b)
liftV f a ra = mkR ir
  where ir b = write ra (f a b)
