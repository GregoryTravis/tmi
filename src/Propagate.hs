{-# LANGUAGE GADTs #-}

module Propagate where 

import Unsafe.Coerce

import History
import Util
import V

propagateFully :: Show w => History w -> Write -> History w
-- propagateFully = propagateOneAtATime
propagateFully = propagateAndApplySequentially

-- Propagate writes one at a time, all the way to the root. So it's sequential.
propagateOneAtATime :: History w -> Write -> History w
propagateOneAtATime h (Write (w:ws)) =
  let w' = propagateOne h first
      h' = newGeneration h w'
      first = Write [w]
      rest = Write ws
   in propagateOneAtATime h' rest
propagateOneAtATime h (Write []) = h

propagateOne :: History w -> Write -> w
propagateOne h (Write []) = r h VRoot
propagateOne h (Write [Write1 VRoot w]) = unsafeCoerce w
propagateOne h (Write [Write1 va a]) =
  let write' = wr h va a
   in propagateOne h write'
propagateOne h (Write writes) = error ("Non-singular write (" ++ show writes ++ ")")

propagateBranching :: History w -> Write -> [w]
propagateBranching h (Write []) = []
propagateBranching h (Write [Write1 VRoot w]) = unsafeCoerce w
propagateBranching h (Write [Write1 va a]) =
  let write' = wr h va a
   in propagateBranching h write'
propagateBranching h (Write writes) = mconcat (map applyOne writes)
  where -- applyOne :: Write1 -> [w]
        applyOne (Write1 va a) =
          let write' = wr h va a
           in propagateBranching h write'

-- Propagate the first write in the list, and repeat until it's a root write.
-- Then advance the history with the new world, and continue with the rest.
-- This is very incorrect.
propagateAndApplySequentially :: History w -> Write -> History w
propagateAndApplySequentially h (Write []) = h
propagateAndApplySequentially h (Write (Write1 VRoot w : writes)) =
  let w' = unsafeCoerce w
      h' = newGeneration h w'
   in propagateAndApplySequentially h' (Write writes)
propagateAndApplySequentially h (Write (Write1 va a : writes)) =
  let write' = wr h va a
      writes' = write' <> Write writes
   in propagateAndApplySequentially h writes'

-- propagate branching, log the resulting worlds, and return the original
-- this failed immediately, e.g. map over different length list
propagateNonSingularAndIgnore :: Show w => History w -> Write -> History w
propagateNonSingularAndIgnore h write =
  let ws = propagateBranching h write
   in eeesp ("propagateNonSingularAndIgnore", ws) h

-- -- Propagate a write to as many worlds as it wants, then return the original state.
-- -- Log the many worlds.
-- propagateFreelyThenDrop :: History w -> Write -> History w
-- propagateFreelyThenDrop h write =
--   let worlds = propagateToMany h write
--    in eesp ("worlds", worlds) h

-- propagateToMany :: History w -> Write -> [w]
-- propagateToMany h (Write [Write1 VRoot w]) = [unsafeCoerce w]
-- propagateToMany h (Write wrs) = mconcat (map (propagateToMany . Write . applyOne) wrs)
--   where applyOne :: Write1 -> Write
--         applyOne (Write1 va a) =
--           let write' = wr h va a
--            in write'

r :: History w -> V a -> a
-- r :: W -> V a -> a
r h VRoot | isEmpty h = error "Can't get the root from an empty history"
          | otherwise = unsafeCoerce (latestState h)
r _ (VConst _ x) = x
r _ (VCheckConst _ x) = x
r h (VApp vfbfa vb) = r h (VSeal (VPartialApp vfbfa vb))
-- TODO not crazy about constructing receivers here
-- r w (VApp vf va) = b
--   where f = r w vf
--         a = r w va
--         -- rb = R b (Receiver $ \b' -> Write [Write1 b'])
--         ra = R a (Receiver "r VApp" $ \a' -> Write [Write1 va a'])
--         rb = f ra
--         b = case rb of R b _ -> b
r h (VSeal vra) = a
  where ra = r h vra
        a = case ra of R a _ -> a
r h (VPartialApp vf va) = paf
  where f = r h vf
        a = r h va
        ra = R a (Receiver "r VPartialApp" $ \a' -> Write [Write1 va a'])
        paf = f ra
-- VUnPartialApp :: (Show a) => (V a -> V rest) -> V (R a -> rest)
-- must return (R a -> rest)
r h (VUnPartialApp vvf) = \ra -> ((r h (vvf (VSeal (VConst "uh" ra)))))
-- r h (VUnPartialApp vvf) = \ra ->     -- ra = -- :: rest
--   let va = VConst "VUnPartialApp" ra
--       vRest = vvf va
--       rest = r h vRest
--    in rest

wr :: History w -> V a -> a -> Write
-- wr :: W -> V a -> a -> Write
-- wr w VRoot _ = undefined "Can't write to root"
wr h v@(VConst s _) _ = error $ "Can't write to a const: " ++ s ++ " " ++ (show v)
wr h v@(VCheckConst s x) x'
  | x == x' = emptyWrite
  | otherwise = error $ "VCheckConst: unequal: " ++ show v ++ " <-- " ++ show x'
-- This was just to ignore what I figured was a equi-const write
-- wr h (VConst s _) _ = emptyWrite
-- Can't
-- wr h (VConst s x) x'
--   | x == x' = error "but ok"
--   | otherwise = error $ "Can't write to a const: " ++ s
wr h (VApp vfbfa vb) b = wr h (VSeal (VPartialApp vfbfa vb)) b
  --where -- write = Write [Write1 vb b']
  --      write = reca a
  --      rbra = r w vfbfa
  --      -- ra = rbra rb
  --      R _ (Receiver reca) = rbra rb
  --      rb = R b (Receiver $ \b' -> Write [Write1 vb b'])
  --      b = r w vb
  --      --b' = undefined
-- Good gravy why is this not needed?
wr h (VPartialApp vf va) _ = error "Why is this error not happening"
wr h (VSeal vra) a = write
  where write = {-eeesp ("REC wr2", s) $-} reca a
        R _ (Receiver s reca) = ra
        ra = r h vra
