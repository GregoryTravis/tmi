{-# Language AllowAmbiguousTypes, FlexibleInstances, NamedFieldPuns, StandaloneDeriving, ScopedTypeVariables,
             TypeApplications, UndecidableInstances #-}

module Recon
( HasRecon(..) ) where

class HasRecon w where
  getRecon :: String -> a
