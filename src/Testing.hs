{-# Language RankNTypes #-}

module Testing
( roundTrip )
where

import Control.Monad (when)
import Data.Dynamic

import Storage
import Ty
import Util

-- Only returning the old + new values so they're the same type, because I
-- don't know how to use (~).
roundTrip :: Reconstitutor -> V w a -> IO [V w a]
roundTrip recon q = do
  let s = qs q
      ss = show s
      rs = read ss
      q' = unqs recon rs
      same = q == q' && s == rs
      check = assertM "roundTrip" same [q, q']
  when (not same) $ do
    msp "===="
    msp q
    msp s
    msp ss
    msp rs
    msp q'
    msp "===="
  msp $ "same " ++ show same
  return check
