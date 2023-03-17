{-# Language GADTs, NamedFieldPuns #-}

module Propagate
( propWrite
, rd
) where

import Data.Maybe (catMaybes)

import Ty
import Util
import V

data Generation = Latest | Frozen Int

-- Pushes a write back one step through a rev?
wr :: [w] -> Generation -> V w b -> b -> Write w
wr ws gen (VBiSeal (Bi qfor qrev)) na =
  let rev = rd ws gen qrev -- R a
      oa = rd ws gen qfor -- a
   in case rev of R rec -> rec na
wr ws gen (VBiSeal bi) a =
  let R rarec = getrev ws gen bi
   in rarec a
wr ws gen q _ = error $ "wr?" -- ++ show q

-- vwr :: Show w => w -> V w b -> b -> Write w
-- vwr w v b = eesp ("wr", v, w) $ wr w v b

getrev :: [w] -> Generation -> Bi w f r -> r
getrev ws gen (Bi qf qr) =
  let r = rd ws gen qr
   in r
getrev ws gen (BiApp bi qa) =
  let rev = getrev ws gen bi
      oa = rd ws gen qa
      ra = R (\a -> Write qa a)
      rb = rev oa ra
   in rb

rd :: [w] -> Generation -> V w a -> a
rd ws gen VRoot = w
rd ws gen (VNice x) = x
rd ws gen (VNamed _ x) = x
rd ws gen (VBiSeal bi) = rdb [w] gen bi
rd ws gen (VDeref vva) = rd [w] gen (rd [w] gen vva)
rd ws gen (VFreeze _ v) = rd w v

rdb :: [w] -> Generation -> Bi w f r -> f
rdb ws gen (Bi qf qr) = rd w qf
rdb ws gen (BiApp bi qa) =
  let for = rdb [w] gen bi
      a = rd [w] gen qa
   in for a

propWrite :: Show w => [w] -> Generation -> Write w -> w
propWrite ws gen write = propWrites ws gen [write]

-- Apply a list of writes to a w. Writes are sequential.
-- For a non-root write, the write is pushed back one step towards the root.
-- For a root write, the new w replaces the old one.

propWrites :: Show w => [w] -> Generation -> w -> [Write w] -> w
propWrites ws gen w [] = w
propWrites ws gen w (VWrite va va' : rest) = propWrites ws gen w (Write va (rd ws gen va') : rest)
propWrites ws gen w (Write VRoot w' : rest) = propWrites ws gen w' rest
propWrites ws gen w (Write va a : rest) = propWrites ws gen w (wr w va a : rest)

-- Faulty attempt I think
-- propWrites :: Show w => [w] -> Generation -> [Write w] -> w
-- propWrites ws gen [] = w
-- propWrites ws gen (VWrite va va' : rest) = propWrites ws gen (Write va (rd ws gen va') : rest)
-- propWrites ws gen (Write VRoot w' : rest) = propWrites w' rest
-- propWrites ws gen (Write va a : rest) = propWrites w (wr w va a : rest)
-- -- TODO append is slow?
-- propWrites w (Writes writes : rest) = propWrites w (writes ++ rest)

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
