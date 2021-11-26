module Lift
( lift1
, lift2 ) where

import Data.Dynamic

import Ty

lift1 :: (Typeable a, Typeable b) => Bi (a -> b) (a -> R a -> R b) -> Q a -> Q b
lift1 bi qa = QBiSeal (BiApp bi qa)

lift2 :: (Typeable a, Typeable b, Typeable c) => Bi (a -> b -> c) (a -> R a -> b -> R b -> R c) -> Q a -> Q b -> Q c
lift2 bi qa qb = QBiSeal (BiApp (BiApp bi qa) qb)
