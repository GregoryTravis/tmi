{-# LANGUAGE ExistentialQuantification, TypeOperators #-}

module Curry (curryMain) where

import Util

-- std lens
-- (a -> b, a -> b -> a)
-- (a -> b -> c, a -> b -> c -> (a, b))
-- alt lens
-- (a -> (b, b -> a))
-- (a -> b -> (c, c -> (a, b)))

data V a = V a
  deriving Show

rv :: V a -> a
rv (V a) = a

data Write = Write | Writes [Write]
  deriving Show

data Write' = Write' Pr | Writes' [Write']
data Pr = forall a. Pr (a, a)
infix 8 <--
(<--) :: a -> a -> Write'
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

--data PrimL a b = PrimL a b

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
