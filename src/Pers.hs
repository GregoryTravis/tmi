{-# Language GADTs, StandaloneDeriving #-}

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

-- works
farg :: Pers a -> String
farg (TLB s) = s
farg (Direct a) = show a
farg (PApp pf pa) = show pf ++ show pa

deriving instance Read (Pers a)
deriving instance Show (Pers a)

readPers :: Pers a -> a
readPers (TLB name) = lookupPers name
readPers (Direct a) = a
readPers (PApp fp ap) = (readPers fp) (readPers ap)
