{-# Language AllowAmbiguousTypes #-}

module Recon
( HasRecon(..) ) where

class HasRecon w where
  getRecon :: String -> a
