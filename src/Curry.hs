{-# LANGUAGE ExistentialQuantification, TypeOperators #-}

module Curry (curryMain) where

import Util

-- std lens
-- (a -> b, a -> b -> a)
-- (a -> b -> c, a -> b -> c -> (a, b))
-- alt lens
-- (a -> (b, b -> a))
-- (a -> b -> (c, c -> (a, b)))
-- curry lens
-- (a -> (b, b -> Write))
-- (a -> b -> (c, c -> Write))
-- but
-- (a -> b -> c)
-- (a -> (b -> c, (b -> c) -> Write))
-- (a -> b -> c, d)
-- (a -> (b -> c -> d, (b -> c -> d) -> Write))
-- but then
--       (b -> c -> d, (b -> c -> d) -> Write) =>
--       b -> (c -> d, (c -> d) -> Write)
-- so then
-- (a -> b -> (c -> d, (c -> d) -> Write))
-- and again
-- (a -> b -> c -> (d, d -> Write))

-- TODO: compact syntax, something like this:
-- hmmPlus x y = withRev rev (x + y)
--   where rev x y z =
--     let [blah blah] 
--      in x <-- x' <>
--         y <-- y'

hmmPlus_for :: Int -> Int -> Int
hmmPlus_for = (+)
hmmPlus_rev :: Int -> Int -> Int -> Write'
hmmPlus_rev x y nZ =
  -- let oZ = hmmPlus_for x y -- if needed
  x <-- x' <>
  y <-- y'
  where x' = nZ `div` 2
        y' = nZ - x'

data Hmm a b = Hmm a b

hmmApp :: (Hmm (a -> b) (a -> c)) -> a -> Hmm b c
hmmApp (Hmm for rev) x = Hmm (for x) (rev x)

hmmPlus0 :: Hmm (Int -> Int -> Int) (Int -> Int -> Int -> Write')
hmmPlus0 = Hmm hmmPlus_for hmmPlus_rev

hmmPlus1 :: Hmm (Int -> Int) (Int -> Int -> Write')
hmmPlus1 = hmmApp hmmPlus0 1

hmmPlus2 :: Hmm (Int) (Int -> Write')
hmmPlus2 = hmmApp hmmPlus1 2

rHmm :: Hmm a b -> a
rHmm (Hmm for _) = for

wHmm :: Hmm a (b -> Write') -> b -> Write'
wHmm (Hmm _ rev) x = rev x

--(<***>) :: Hmm (a -> r) (a -> r -> Write') -> a -> Hmm r (r -> Write')
(<***>) :: Hmm (t -> a) (t -> b) -> t -> Hmm a b
(Hmm for rev) <***> x = Hmm (for x) (rev x)

hmmVal :: Hmm Int (Int -> Write')
hmmVal = (Hmm hmmPlus_for hmmPlus_rev) <***> 1 <***> 2

hmmRead :: Int
hmmRead = rHmm hmmVal

hmmWrote :: Write'
hmmWrote = wHmm hmmVal 30

{-
-- This looks the same as L, below, but without the Vs -- ?
data Hmm a r s = Hmm (a -> r) (a -> s)

-- No
hmmPlus0 :: Hmm Int (Int -> Int) (Int -> Int -> Write')
hmmPlus0 = Hmm hmmPlus_for hmmPlus_rev

hmmApp :: Hmm a1 (a2 -> r) (a2 -> s) -> a1 -> Hmm a2 r s
hmmApp (Hmm for rev) x = Hmm (for x) (rev x)

hmmPlus1 :: Hmm Int Int (Int -> Write')
hmmPlus1 = hmmApp hmmPlus0 1
-- ish:
-- hmmPlus1 = Hmm (Int -> Int) (Int -> (Int -> Write'))

--hmmPlus2 = hmmApp hmmPlus1 2
-}

--------

data V a = V a
  deriving Show

rv :: V a -> a
rv (V a) = a

data Write = Write | Writes [Write]
  deriving Show

data Write' = Write' Pr | Writes' [Write']
data Pr = forall a. Show a => Pr (a, a)

-- instance Show Pr where
--   show (Pr (x, y)) = show (x, y)

infix 8 <--
(<--) :: Show a => a -> a -> Write'
x <-- y = Write' (Pr (x, y))

-- TODO: This is a super-dumb implementation, should flatten as we go
instance Semigroup Write' where
  w <> w' = Writes' [w, w']
instance Monoid Write' where
  mempty = Writes' []

cwv :: V a -> a -> Write
cwv _ _ = Write

-- data U a = U a

-- ru :: U a -> a
-- ru (U a) = a

-- (<*>) :: f (a -> b) -> f a -> f b

plus_for :: V Int -> V Int -> V Int
plus_for vx vy = V (rv vx + rv vy)
-- last two are old output, new output -- but we could calculate the old output from the new output, too
plus_rev :: V Int -> V Int -> V Int -> V Int -> Write
-- vSum unused
plus_rev vx vy vSum nSum = Writes [cwv vx x', cwv vy y']
  where x' = rv nSum `div` 2
        y' = rv nSum - x'

--data L a b c = L (a -> b) (a -> c)
data L a b = L a b

primPlus_for :: Int -> Int -> Int
primPlus_for = (+)
primPlus_rev :: Int -> Int -> Int -> Int -> Write'
--primPlus_rev x y oZ = Writes' [Write' $ Pr (x, x'), Write' $ Pr (y, y')]
primPlus_rev x y _oZ nZ =
  x <-- x' <>
  y <-- y'
  where x' = nZ `div` 2
        y' = nZ - x'

(<**>) = lft_for

lft_for :: V (a -> b) -> (V a -> V b)
lft_for vf = \va -> V $ (rv vf) (rv va)

-- TODO: Same?
lft_rev = lft_for

primPlus_for0 :: V Int -> V (Int -> Int)
primPlus_for0 = lft_for (V primPlus_for)

primPlus_for1 :: V Int -> V Int -> V Int
primPlus_for1 = lft_for . primPlus_for0

primPlus_rev0 :: V Int -> V (Int -> Int -> Int -> Write')
primPlus_rev0 = lft_rev (V primPlus_rev)

primPlus_rev1 :: V Int -> V Int -> V (Int -> Int -> Write')
primPlus_rev1 = lft_rev . primPlus_rev0

(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
f .: g = \x y -> f (g x y)

(.::) :: (d -> e) -> (a -> b -> c -> d) -> (a -> b -> c -> e)
f .:: g = \x y z -> f (g x y z)

primPlus_rev2 :: V Int -> V Int -> V Int -> V (Int -> Write')
primPlus_rev2 = lft_rev .: primPlus_rev1

-- TODO: do we want V Write'?
primPlus_rev3 :: V Int -> V Int -> V Int -> V Int -> V Write'
--primPlus_rev3 vx vy vz vnz = (lft_rev (primPlus_rev2 vx vy vz) vnz)
primPlus_rev3 = lft_rev .:: primPlus_rev2

aPrimV :: V Int
aPrimV = primPlus_for1 (V 1) (V 2)

aPrimVRev :: V Int -> V Write'
aPrimVRev = primPlus_rev3 (V 1) (V 2) aPrimV

aPrimVRevWrote :: V Write'
aPrimVRevWrote = aPrimVRev (V 30)

aPrimV' :: V Int
aPrimV' = (V primPlus_for) <**> (V 1) <**> (V 2)

aPrimVRev' :: V (Int -> Write')
aPrimVRev' = (V primPlus_rev) <**> (V 1) <**> (V 2) <**> aPrimV

aPrimVRevWrote' :: V Write'
aPrimVRevWrote' = aPrimVRev' <**> (V 30)

plus :: L (V Int -> V Int -> V Int) (V Int -> V Int -> V Int -> V Int -> Write)
plus = L plus_for plus_rev

app :: L (V a -> b) (V a -> c) -> V a -> L b c
app (L for rev) vx = L (for vx) (rev vx)

foo :: L (V Int -> V Int) (V Int -> V Int -> V Int -> Write)
foo = app plus (V 1)

-- foo'' :: V Int -> L (V Int) (V Int -> Write)
-- foo'' = app foo
-- foo' :: L (V Int) (V Int -> Write)
-- foo' =
--   let vy = V 2 
--    in case foo of L for rev -> L (for vy) (rev vy)

foo' :: L (V Int) (V Int -> V Int -> Write)
foo' = app foo (V (2::Int))

foo'2 :: L (V Int) (V Int -> V Int -> Write)
foo'2 = app (app plus (V 1)) (V (2::Int))

rL :: L (V a) b -> a
rL (L vx _) = rv vx

-- No: we need a new output value
wL :: L (V a) (V a -> V a -> Write) -> V a -> Write
wL (L oldX writer) newX = writer oldX newX

theSum :: Int
theSum = rL foo'2

theWrote :: Write
theWrote = wL foo'2 (V 20)

-- TODO
-- - lift primPlus_* as plus
--   + lift individuals
--   - maybe use (V Write') everywhere?
--   - lift them together
--   - make plus (the L) out of those plain funs

curryMain = do
  msp "curry hi"
