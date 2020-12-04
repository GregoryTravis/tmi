{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Lift
( hoist_0_1
, hoist_1_1
, hoist_2_1
, hoist_1_2
, konstV
) where

import Hash
import Internal
import Util

-- Lifting the dynamic back into a static. This makes sure the types are right.
-- If the lifters don't have any bugs, then all the Dynamic conversions will
-- succeed, and outside code can't mess that up. This and konstV are the only
-- things that user code should have access to.
--
-- I had to tie the knot between the N and its outputs (the argument of applySD
-- is a list of dynamicized copies of the returned nodes; the returned nodes
-- point to the N returned by applySD). I couldn't find any other place where
-- it would let me call dyv. It works here because we're return the Vs, so we
-- know the type of (V n i), so dyv will take it. I would rather it be done
-- otherwise but this is the only way I could make it work.
hoist_0_1 :: Nice a => F0 a -> V a
hoist_0_1 f =
  let n = applySD (lift_0_1 f) [] [dyv v]
      v = V n 0
   in v

hoist_1_1 :: (Nice a, Nice b) => F a b -> (V a -> V b)
hoist_1_1 f va =
  let n = (applySD (lift_1_1 (noisy1 f)) [dyv va] [dyv v])
      v = V n 0
   in v

hoist_2_1 :: (Nice a, Nice b, Nice c) => F2 a b c -> (V a -> V b -> V c)
hoist_2_1 f va vb =
  let n = applySD (lift_2_1 (noisy2 f)) [dyv va, dyv vb] [dyv v]
      v = V n 0
   in v

hoist_1_2 :: (Nice a, Nice b, Nice c) => F_1_2 a b c -> (V a -> (V b, V c))
hoist_1_2 f va = (vb, vc)
  where n = applySD (lift_1_2 (noisy_1_2 f)) [dyv va] [dyv vb, dyv vc]
        vb = V n 0
        vc = V n 1

konstV :: (Show a, Nice a) => a -> V a
konstV x = hoist_0_1 $ F0 { name0, ffor0 = x, frev0 = undefined }
  where name0 = hash x

-- For lifting plain forward and reverse functions.  Functions are curried,
-- tuples turned into lists, and inputs in particular are turned into Vs.
--
-- Forward functions are curried, and then the input tuple and the output
-- singletons are turned into [Dynamic]s. (Output lists are typically length one
-- but don't have to be.) Any forward function becomes (Ds -> Ds).
--
-- Reverse functions are the same, except it takes the (old) input tuple and
-- new output tuple and returns the new input tuple, all in the form of
-- [Dynamic]. Any reverse function becomes (Ds -> Ds -> Ds).
lift_0_1 :: Nice a => F0 a -> S
lift_0_1 (F0 {..}) = S {..}
  where for [] = [dy $ ffor0]
        rev = undefined
        names = name0

lift_1_1 :: (Nice a, Nice b) => F a b -> S
lift_1_1 (F {..}) = S {..}
  where for = dys1 . ffor . undys1
        rev [doin0] [dnout0] = [dy nin0]
          where nin0 = frev (undy doin0) (undy dnout0)
        names = name

lift_2_1 :: (Nice a, Nice b, Nice c) => F2 a b c -> S
lift_2_1 (F2 {..}) = S {..}
  where for = dys1 . uncurry ffor2 . undys2
        rev [doin0, doin1] [dnout0] = [dy nin0, dy nin1]
          where (nin0, nin1) = frev2 (undy doin0) (undy doin1) (undy dnout0)
        names = name2

lift_1_2 :: (Nice a, Nice b, Nice c) => F_1_2 a b c -> S
lift_1_2 (F_1_2 {..}) = S {..}
  --where for dins = dys2 $ ffor_1_2 (undys1 dins)
  where for = dys2 . ffor_1_2 . undys1
        rev doins dnouts = dys1 $ frev_1_2 (undys1 doins) (undys2 dnouts)
        names = name_1_2

-- Apply an S to args to make an N.
applySD :: S -> DVs -> DVs -> N
applySD s dvs outputDVs = N { n_s = s, args = dvs, results = outputDVs }

-- Convenicenes for converting between typed and untyped value bundles
dys1 :: (Nice a) => a -> [D]
dys1 a = [dy a]
undys1 :: (Nice a) => [D] -> a
undys1 [da] = undy da
dys2 :: (Nice a, Nice b) => (a, b) -> [D]
dys2 (a, b) = [dy a, dy b]
undys2 :: (Nice a, Nice b) => [D] -> (a, b)
undys2 [da, db] = (undy da, undy db)

-- TODO move this out?

-- A wrapper takes the input and output, does something with those, and returns the output.
-- TODO shouldn't have to take care to return the output
wrap1 :: (a -> b -> b) -> (a -> b) -> (a -> b)
wrap1 wrapper f a = let b = f a in wrapper a b
wrap2 :: (a -> b -> c -> c) -> (a -> b -> c) -> (a -> b -> c)
wrap2 wrapper f a b = let c = f a b in wrapper a b c
wrap3 :: (a -> b -> c -> d -> d) -> (a -> b -> c -> d) -> (a -> b -> c -> d)
wrap3 wrapper r a b c = let d = r a b c in wrapper a b c d

wrap_for_1_1 = wrap1
wrap_rev_1_1 = wrap2
wrap_for_2_1 = wrap2
wrap_rev_2_1 = wrap3
wrap_for_1_2 = wrap1
wrap_rev_1_2 = wrap2

-- TODO should be renamed wrap_2_1 when F2 becomes F_2_1, etc
noisy1 :: (Nice a, Nice b) => F a b -> F a b
noisy1 (F {..}) = F name ffor' frev'
  where ffor' = wrap_for_1_1 (\a b -> eesp (name, "for", a, "->", b) b) ffor
        frev' = wrap_rev_1_1 (\a b a' -> eesp (name, "rev", a, b, "->", a') a') frev
noisy2 :: (Nice a, Nice b, Nice c) => F2 a b c -> F2 a b c
noisy2 (F2 {..}) = F2 name2 ffor2' frev2'
  where ffor2' = wrap_for_2_1 (\a b c -> eesp (name2, "for", a, b, "->", c) c) ffor2
        frev2' = wrap_rev_2_1 (\a b c (a', b') -> eesp (name2, "rev", a, b, c, "->", (a', b')) (a', b')) frev2
noisy_1_2 (F_1_2 {..}) = F_1_2 name_1_2 ffor_1_2' frev_1_2'
  where ffor_1_2' = wrap_for_1_2 (\a bc -> eesp (name_1_2, "for", a, "->", bc) bc) ffor_1_2
        frev_1_2' = wrap_rev_1_2 (\a bc bc' -> eesp (name_1_2, "rev", a, bc, "->", bc') bc') frev_1_2
