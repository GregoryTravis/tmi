{-# Language ScopedTypeVariables, StandaloneDeriving, TypeApplications #-}

module VReadShow where

import Recon
import Storage
import Ty

instance Show (V w a) where
  show v = show (qs v)

instance HasRecon w => Read (V w a) where
  readsPrec i s = readsPrecer (getRecon @w) i s
