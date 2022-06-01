{-# Language GADTs, NamedFieldPuns #-}

module Propagate
( propWrite
, rd
) where

import Data.Maybe (catMaybes)

import Ty
import Util
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
wr w q _ = error $ "wr?" -- ++ show q

-- vwr :: Show w => w -> V w b -> b -> Write w
-- vwr w v b = eesp ("wr", v, w) $ wr w v b

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
rd w (VDeref vva) = rd w (rd w vva)

rdb :: w -> Bi w f r -> f
rdb w (Bi qf qr) = rd w qf
rdb w (BiApp bi qa) =
  let for = rdb w bi
      a = rd w qa
   in for a

propWrite :: Show w => w -> Write w -> w
propWrite w write = propWrites w [write]

propWrites :: Show w => w -> [Write w] -> w
propWrites w [] = w
propWrites w (VWrite va va' : rest) = propWrites w (Write va (rd w va') : rest)
propWrites w (Write VRoot w' : rest) = propWrites w' rest
propWrites w (Write va a : rest) = propWrites w (wr w va a : rest)
-- TODO append is slow?
propWrites w (Writes writes : rest) = propWrites w (writes ++ rest)

-- Depth-first traversal, but updating the w we carry along each time we get a root write.
-- This might actually be correct for cases where writes don't overlap? I don't know.
-- propWrite :: w -> Write w -> w
-- propWrite w (VWrite va va') = propWrite w (Write va (rd w va'))
-- propWrite w (Write VRoot w') = w'
-- propWrite w (Writes []) = w
-- propWrite w (Writes (Write VRoot w' : writes)) = propWrite w' (Writes writes)
-- propWrite w (Writes (VWrite VRoot vw' : writes)) =
--   error "Implementation present but suspended, check source"
--   -- This is actually correct but I think we should probably never do this?
--   -- propWrite w (Writes (Write VRoot (rd w vw') : writes))
-- propWrite w (Writes (Writes writes : writes2))

{-
-- One step
propWrite :: w -> Write w -> Write w
propWrite w (Write qa a) = wr w qa a
propWrite w (VWrite qa qa') = propWrite w (Write qa (rd w qa'))

-- One step for Write/VWrite; for Writes, do recursively and collect
propWriteSome :: w -> Write w -> [Write w]
-- Use propWrite here
propWriteSome w (Write qa a) = [wr w qa a]
-- Use propWrite here
propWriteSome w (VWrite qa qa') = [propWrite w (Write qa (rd w qa'))]
propWriteSome w (Writes ws) = concat $ map (propWriteSome w) ws

-- Concat write onto its further propagations, only stopping at root
propWriteFully :: w -> Write w -> [Write w]
propWriteFully w write@(Write VRoot _) = [write]
propWriteFully w write@(VWrite VRoot _) = error "propWriteFully: really?"
propWriteFully w write = write : (concat $ map (propWriteFully w) (propWriteSome w write))

-- Prop all the way then collect all the root writes
propToRoots :: w -> Write w -> [w]
propToRoots w write =
  let writes = propWriteFully w write
   in catMaybes $ map ifRoot writes

-- Prop all the way then collect all the root writes and it has to be 0 or 1
propToRoot :: Show w => w -> Write w -> w
propToRoot w write = -- one (propToRoots w write)
  case propToRoots w write of [w'] -> w'
                              [] -> w
                              bad -> eesp ("too many writes", bad) undefined
  where one [x] = x
        one xs = error $ "there can be only one " ++ show xs
-}

ifRoot :: Write w -> Maybe w
ifRoot (Write VRoot w) = Just w
ifRoot (VWrite VRoot w) = error "ifRoot: really?"
ifRoot _ = Nothing
