{-# Language AllowAmbiguousTypes, ExistentialQuantification, GADTs, RankNTypes, ScopedTypeVariables, PartialTypeSignatures, TypeApplications, KindSignatures #-}

module Reddit (redditMain) where

import Data.Dynamic
import Data.Kind (Type)
import Data.Proxy
import Data.Maybe (fromJust)
import Type.Reflection
import Unsafe.Coerce (unsafeCoerce)

import Util

data W
data Write
data R a = R (a -> Write)

data D f r where
  D :: C f -> C r -> D f r
  DApp :: D (a -> b) (a -> R a -> c) -> C a -> D b c
--           ^--------^------^------------^---------- 'a' is not in the output type

data C a where
  -- CRoot :: C W
  -- CNice :: (Eq a, Show a, Read a, Typeable a) => a -> C a
  CNamed :: String -> a -> C a
  CDSeal :: D a (R a) -> C a

instance Eq (D f r) where
  D qf qr == D qf' qr' = qf == qf' && qr == qr'
  -- Error: Couldn't match type ‘a1’ with ‘a’
  -- DApp bi q == DApp bi' q' = bi == bi' && q == q'

instance Eq (C a) where
  -- CRoot == CRoot = True
  -- CNice x == CNice y = x == y
  CNamed name _ == CNamed name' _ = name == name'
  CDSeal bi == CDSeal bi' = bi == bi'

redditMain = msp "hi reddit"
