{-# Language AllowAmbiguousTypes #-}

module Recon
( HasRecon(..) ) where

import Ty

class HasRecon w where
  getRecon :: String -> a
