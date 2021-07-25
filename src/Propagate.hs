module Propagate where 

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
propagateOne h (Write writes) = error ("Non-singular write (" ++ (show writes) ++ ")")

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

