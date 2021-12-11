{-# LANGUAGE GADTs, RankNTypes #-}

module Monad
( Core(..) ) where

import Util
import V
import Ty

-- The core language consists of steps, each with its continuation.
data Core w where
  Assign :: Write w -> (() -> Core w) -> Core w
  Call :: forall a w. IO a -> (a -> Core w) -> Core w
  Done :: Core w

-- data Step w = forall a. Read a => Step (IO a) (a -> Core w)
