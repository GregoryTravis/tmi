{-# Language GADTs, NamedFieldPuns #-}

module Propagate
( propToRoots ) where

import Data.Maybe (catMaybes)

import V
import Ty

wr :: W -> V b -> b -> Write
wr w (VBiSeal (Bi qfor qrev)) na =
  let rev = rd w qrev -- R a
      oa = rd w qfor -- a
   in case rev of R rec -> rec na
wr w (VBiSeal bi) a =
  let R rarec = getrev w bi
   in rarec a
wr w q _ = error $ "wr " ++ show q

getrev :: W -> Bi f r -> r
getrev w (Bi qf qr) =
  let r = rd w qr
   in r
getrev w (BiApp bi qa) =
  let rev = getrev w bi
      oa = rd w qa
      ra = R (\a -> Write qa a)
      rb = rev oa ra
   in rb

rd :: W -> V a -> a
rd w VRoot = w
rd w (VNice x) = x
rd w (VNamed _ x) = x
rd w (VBiSeal bi) = rdb w bi

rdb :: W -> Bi f r -> f
rdb w (Bi qf qr) = rd w qf
rdb w (BiApp bi qa) =
  let for = rdb w bi
      a = rd w qa
   in for a

propWrite :: W -> Write -> Write
propWrite w (Write qa a) = wr w qa a

propWriteSome :: W -> Write -> [Write]
propWriteSome w (Write qa a) = [wr w qa a]
propWriteSome w (Writes ws) = concat $ map (propWriteSome w) ws

propWriteFully :: W -> Write -> [Write]
propWriteFully w write@(Write VRoot _) = [write]
propWriteFully w write = write : (concat $ map (propWriteFully w) (propWriteSome w write))

propToRoots :: W -> Write -> [W]
propToRoots w write =
  let writes = propWriteFully w write
   in catMaybes $ map ifRoot writes

ifRoot :: Write -> Maybe W
ifRoot (Write VRoot w) = Just w
ifRoot _ = Nothing
