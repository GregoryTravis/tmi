module Testing
( roundTrip ) where

import Data.Dynamic

import Storage
import Ty
import Util

roundTrip :: (Typeable a, Typeable w) => Reconstitutor -> V w a -> IO [V w a]
roundTrip recon q = do
  let s = qs q
      ss = show s
      rs = read ss
      q' = sq recon rs
      check = assertM "roundTrip" (q == q' && s == rs) [q, q']
  msp "===="
  msp q
  msp s
  msp ss
  msp rs
  msp q'
  msp "===="
  return check
