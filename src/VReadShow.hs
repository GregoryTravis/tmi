{-# Language ScopedTypeVariables, StandaloneDeriving, TypeApplications #-}

module VReadShow where

import Recon
import Storage
import Ty

instance Show (V w a) where
  show v = show (qs v)

instance HasRecon w => Read (V w a) where
  readsPrec i s = readsPrecer (getRecon @w) i s

instance Show (Write w) where
  show (Write va a) = "(Write " ++ show va ++ "()"
  show (VWrite va va') = "(VWrite " ++ show va ++ " " ++ show va' ++ ")"
