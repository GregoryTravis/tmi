{-# Language GADTs #-}

module Pers
( Pers(..)
, readPers
, lookupPers ) where

import Unsafe.Coerce

import Util

plus1 = (+1)

lookupPers :: String -> a
lookupPers "plus1" = unsafeCoerce plus1

data Pers a where
  TLB :: String -> Pers a
  Direct :: (Show a, Read a) => a -> Pers a
  PApp :: Pers (a -> b) -> Pers a -> Pers b

readPers :: Pers a -> a
readPers (TLB name) = lookupPers name
readPers (Direct a) = a
readPers (PApp fp ap) = (readPers fp) (readPers ap)
