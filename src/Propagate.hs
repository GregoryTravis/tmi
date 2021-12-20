{-# Language GADTs, NamedFieldPuns #-}

module Propagate
( propToRoot ) where

import Data.Maybe (catMaybes)

import Ty
import V

-- Pushes a write back one step through a rev?
wr :: w -> V w b -> b -> Write w
wr w (VBiSeal (Bi qfor qrev)) na =
  let rev = rd w qrev -- R a
      oa = rd w qfor -- a
   in case rev of R rec -> rec na
wr w (VBiSeal bi) a =
  let R rarec = getrev w bi
   in rarec a
wr w q _ = error $ "wr " ++ show q

getrev :: w -> Bi w f r -> r
getrev w (Bi qf qr) =
  let r = rd w qr
   in r
getrev w (BiApp bi qa) =
  let rev = getrev w bi
      oa = rd w qa
      ra = R (\a -> Write qa a)
      rb = rev oa ra
   in rb

rd :: w -> V w a -> a
rd w VRoot = w
rd w (VNice x) = x
rd w (VNamed _ x) = x
rd w (VBiSeal bi) = rdb w bi

rdb :: w -> Bi w f r -> f
rdb w (Bi qf qr) = rd w qf
rdb w (BiApp bi qa) =
  let for = rdb w bi
      a = rd w qa
   in for a

propWrite :: w -> Write w -> Write w
propWrite w (Write qa a) = wr w qa a

propWriteSome :: w -> Write w -> [Write w]
propWriteSome w (Write qa a) = [wr w qa a]
propWriteSome w (Writes ws) = concat $ map (propWriteSome w) ws

propWriteFully :: w -> Write w -> [Write w]
propWriteFully w write@(Write VRoot _) = [write]
propWriteFully w write = write : (concat $ map (propWriteFully w) (propWriteSome w write))

propToRoots :: w -> Write w -> [w]
propToRoots w write =
  let writes = propWriteFully w write
   in catMaybes $ map ifRoot writes

propToRoot :: Show w => w -> Write w -> w
propToRoot w write = one (propToRoots w write)
  where one [x] = x
        one xs = error $ "there can be only one " ++ show xs

ifRoot :: Write w -> Maybe w
ifRoot (Write VRoot w) = Just w
ifRoot _ = Nothing
