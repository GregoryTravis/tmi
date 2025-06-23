module Fd
( DR(..)
, mkFds
, isFdOf
, fdTests ) where

import Name
import Rec
import Rel
import Tst
import Value
import Util

{- Rules
If A -> B, then A -> B-* (wait is * in A?) (no)
If A -> B, then A+*->B (wait is * in B?) (no)
You can increase the domain and decrease the range.
If A->B and C->D, then A+C->B+D (first add a and c to each other?)
If A->B and B->C then A->C

In particular:
A->B => A+B->B
A->B => A->B-A
-}

-- Names of the domain and range columns
data DR = DR [Name] [Name]

data Fds = Fds [DR]

mkFds :: [DR] -> Fds
mkFds = Fds

-- Determine if a candidate fd dr belongs to fds.
-- If dr can be derived from the fds in fds, then yes.
isFdOf :: Fds -> DR -> Bool
isFdOf (Fds fds) dr = any (\fd -> canDeriveFrom fd dr) fds

canDeriveFrom :: DR -> DR -> Bool
canDeriveFrom (DR fromD fromR) (DR toD toR) =
  -- to's domain contains from's domain
  -- from's range contains to's domain
  subset fromD toD && subset toR fromR
  where subset sub sup = all (\x -> elem x sup) sub

fdTests = do
  let fds0 = mkFds [DR ["a", "b"] ["c", "d"]]
  tst $ isFdOf fds0 (DR ["a", "b"] ["c", "d"])
  tst $ isFdOf fds0 (DR ["a", "b", "e"] ["c", "d"])
  tst $ isFdOf fds0 (DR ["a", "b"] ["c"])
  tst $ not $ isFdOf fds0 (DR ["a", "b"] ["c", "d", "e"])
  tst $ not $ isFdOf fds0 (DR ["a"] ["c", "d"])
