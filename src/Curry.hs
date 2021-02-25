{-# LANGUAGE ExistentialQuantification, TypeOperators, RecordWildCards #-}

module Curry (curryMain) where

import Util

{-
+ V out of Receiver
+ liftT2 by composition? ish
+ U, somehow: we want static guarantee we don't pass a U as a V, but vice versa is ok
- --> etc meaning V a -> UR b etc
- write cascade
-}

data V a = V a
  deriving Show

-- TODO these would be united with subclassing
r :: V a -> a
r (V x) = x

-- This is TMI-specific, but in unlifted mode, contains no TMI machinery, like this:
data Receiver a = Receiver
  deriving Show
-- It could also be (Receiver (V a)) in lifted mode

-- Bidirectional receiver
data R a = R (Receiver a) a
  deriving Show

-- In lifted mode we'd stuff the V a in there or something
mkR :: V a -> R a
mkR va = R Receiver (r va)

data Write = forall a. Show a => Write { receiver :: Receiver a, value :: a, shower :: String }
instance Show Write where
  show (Write {..}) = shower

mkWrite :: Show a => Receiver a -> a -> Write
mkWrite rx x = Write rx x (show ("Write", rx, x))
type Writes = [Write]

infix 8 <--
(<--) :: Show a => Receiver a -> a -> [Write]
rx <-- x = [mkWrite rx x]

-- This is still haskell, just a lens, no tmi machinery other than R
data F a b = F a b

plus_for :: Int -> Int -> Int
plus_for = (+)
plus_rev :: R Int -> R Int -> Int -> Writes
plus_rev (R rx x) (R ry y) nZ =
  -- let oZ = hmmPlus_for x y -- if needed
  rx <-- x' <>
  ry <-- y'
  where x' = nZ `div` 2
        y' = nZ - x'

plus = F plus_for plus_rev

plus_for' :: Int -> Int -> Int
plus_for' = (+)
plus_rev' :: R Int -> R Int -> Int -> Writes
plus_rev' (R rx x) (R _ y) nZ =
  -- let oZ = hmmPlus_for x y -- if needed
  rx <-- x'
  where x' = nZ - y

plus' :: F (Int -> Int -> Int) (R Int -> R Int -> Int -> Writes)
plus' = F plus_for' plus_rev'

-- TODO aliases for these
-- TODO that last one is a V
--
-- The first, commented-out signature is for lifting arity-1 functions, but
-- replacing (b -> Writes) with c seems to let it work on arity-2 by repeated
-- application.
-- liftT :: (V (F (a -> b) (R a -> b -> Writes))) -> (V a -> V (F b (b -> Writes)))
liftT :: (V (F (a -> b) (R a -> c))) -> (V a -> V (F b (c)))
liftT (V (F for rev)) va = V $ F for' rev'
  where for' = for (r va)
        rev' = rev (mkR va)

-- By analogy with Applicative
(<**>) :: V (F (a -> b) (R a -> c)) -> V a -> V (F b c)
(<**>) = liftT

(<$$>) :: F (a -> b) (R a -> c) -> V a -> V (F b c)
f <$$> x = (V f) <**> x

-- Can it be done compositionally?
liftT2 :: (V (F (a -> b -> c) (R a -> R b -> c -> Writes))) -> (V a -> V b -> V (F c (c -> Writes)))
liftT2 (V (F for rev)) va vb = V $ F for' rev'
  where for' = for (r va) (r vb)
        rev' nC = rev (mkR va) (mkR vb) nC

-- This hyar is almost composition
-- Again, first sig is overspecified but correct for arity 2 (?)
-- liftT2' :: V (F (a -> b -> c) (R a -> R b -> c -> Writes)) -> V a -> V b -> V (F c (c -> Writes))
liftT2' :: V (F (a1 -> a2 -> b) (R a1 -> R a2 -> c)) -> V a1 -> V a2 -> V (F b c)
-- These are all the same
-- liftT2' f x y = liftT (liftT f x) y
-- liftT2' f x = liftT (liftT f x)
liftT2' f = liftT . liftT f

la :: V (F Int (Int -> Writes))
la = liftT2 (V plus) (V 1) (V 2)
la' :: V (F Int (Int -> Writes))
la' = liftT2' (V plus) (V 1) (V 2)

la1 :: V (F (Int -> Int) (R Int -> Int -> Writes))
la1 = (V plus) <**> (V 1)
la1' :: V (F (Int -> Int) (R Int -> Int -> Writes))
la1' = plus <$$> (V 1)

la2 :: V (F Int (Int -> Writes))
la2 = la1 <**> (V 2)
la2' :: V (F Int (Int -> Writes))
la2' = plus <$$> (V 1) <**> (V 2)

curryMain = do
  msp $ case la of Curry.V (Curry.F x _) -> x
  msp $ case la' of Curry.V (Curry.F x _) -> x
  msp $ case la2 of Curry.V (Curry.F x _) -> x
  msp $ case la2' of Curry.V (Curry.F x _) -> x

  msp $ (case la of Curry.V (Curry.F x y) -> y) 30
  msp $ (case la' of Curry.V (Curry.F x y) -> y) 30
  msp $ (case la2 of Curry.V (Curry.F x y) -> y) 30
  msp $ (case la2' of Curry.V (Curry.F x y) -> y) 30
  msp "curry hi"

-- la :: F (V (Int -> Int)) (V (R Int -> Int -> Int -> Writes))
-- la = plus `app` (V 1) -- `app` (V 2)

-- la' :: F (V (Int)) (V (Int -> Writes))
-- la' = la `app` (V 2)

-- rF :: F (V a) b -> a
-- rF (F va _) = r va

-- wF :: F a (V (b -> Writes)) -> V b -> Writes
-- wF (F _ rev) vb = (r rev) (r vb)

-- yep = rF la'
-- yup = wF la' (V 30)

--{-
---- std lens
---- (a -> b, a -> b -> a)
---- (a -> b -> c, a -> b -> c -> (a, b))
---- alt lens
---- (a -> (b, b -> a))
---- (a -> b -> (c, c -> (a, b)))
---- curry lens
---- (a -> (b, b -> Write))
---- (a -> b -> (c, c -> Write))
---- but
---- (a -> b -> c)
---- (a -> (b -> c, (b -> c) -> Write))
---- (a -> b -> c, d)
---- (a -> (b -> c -> d, (b -> c -> d) -> Write))
---- but then
----       (b -> c -> d, (b -> c -> d) -> Write) =>
----       b -> (c -> d, (c -> d) -> Write)
---- so then
---- (a -> b -> (c -> d, (c -> d) -> Write))
---- and again
---- (a -> b -> c -> (d, d -> Write))

---- TODO: compact syntax, something like this:
---- hmmPlus x y = withRev rev (x + y)
----   where rev x y z =
----     let [blah blah] 
----      in x <-- x' <>
----         y <-- y'

--hmmPlus_for :: Int -> Int -> Int
--hmmPlus_for = (+)
--hmmPlus_rev :: Int -> Int -> Int -> Write'
--hmmPlus_rev x y nZ =
--  -- let oZ = hmmPlus_for x y -- if needed
--  x <-- x' <>
--  y <-- y'
--  where x' = nZ `div` 2
--        y' = nZ - x'

--data Hmm a b = Hmm a b

--hmmApp :: (Hmm (a -> b) (a -> c)) -> a -> Hmm b c
--hmmApp (Hmm for rev) x = Hmm (for x) (rev x)

--hmmPlus0 :: Hmm (Int -> Int -> Int) (Int -> Int -> Int -> Write')
--hmmPlus0 = Hmm hmmPlus_for hmmPlus_rev

--hmmPlus1 :: Hmm (Int -> Int) (Int -> Int -> Write')
--hmmPlus1 = hmmApp hmmPlus0 1

--hmmPlus2 :: Hmm (Int) (Int -> Write')
--hmmPlus2 = hmmApp hmmPlus1 2

--rHmm :: Hmm a b -> a
--rHmm (Hmm for _) = for

--wHmm :: Hmm a (b -> Write') -> b -> Write'
--wHmm (Hmm _ rev) x = rev x

----(<***>) :: Hmm (a -> r) (a -> r -> Write') -> a -> Hmm r (r -> Write')
--(<***>) :: Hmm (t -> a) (t -> b) -> t -> Hmm a b
--(Hmm for rev) <***> x = Hmm (for x) (rev x)

--hmmVal :: Hmm Int (Int -> Write')
--hmmVal = (Hmm hmmPlus_for hmmPlus_rev) <***> 1 <***> 2

--hmmRead :: Int
--hmmRead = rHmm hmmVal

--hmmWrote :: Write'
--hmmWrote = wHmm hmmVal 30

--{-
---- This looks the same as L, below, but without the Vs -- ?
--data Hmm a r s = Hmm (a -> r) (a -> s)

---- No
--hmmPlus0 :: Hmm Int (Int -> Int) (Int -> Int -> Write')
--hmmPlus0 = Hmm hmmPlus_for hmmPlus_rev

--hmmApp :: Hmm a1 (a2 -> r) (a2 -> s) -> a1 -> Hmm a2 r s
--hmmApp (Hmm for rev) x = Hmm (for x) (rev x)

--hmmPlus1 :: Hmm Int Int (Int -> Write')
--hmmPlus1 = hmmApp hmmPlus0 1
---- ish:
---- hmmPlus1 = Hmm (Int -> Int) (Int -> (Int -> Write'))

----hmmPlus2 = hmmApp hmmPlus1 2
---}

----------

--data V a = V a
--  deriving Show

--rv :: V a -> a
--rv (V a) = a

--data Write = Write | Writes [Write]
--  deriving Show

--data Write' = Write' Pr | Writes' [Write']
--data Pr = forall a. Show a => Pr (a, a)

---- instance Show Pr where
----   show (Pr (x, y)) = show (x, y)

--infix 8 <--
--(<--) :: Show a => a -> a -> Write'
--x <-- y = Write' (Pr (x, y))

---- TODO: This is a super-dumb implementation, should flatten as we go
--instance Semigroup Write' where
--  w <> w' = Writes' [w, w']
--instance Monoid Write' where
--  mempty = Writes' []

--cwv :: V a -> a -> Write
--cwv _ _ = Write

---- data U a = U a

---- ru :: U a -> a
---- ru (U a) = a

---- (<*>) :: f (a -> b) -> f a -> f b

--plus_for :: V Int -> V Int -> V Int
--plus_for vx vy = V (rv vx + rv vy)
---- last two are old output, new output -- but we could calculate the old output from the new output, too
--plus_rev :: V Int -> V Int -> V Int -> V Int -> Write
---- vSum unused
--plus_rev vx vy vSum nSum = Writes [cwv vx x', cwv vy y']
--  where x' = rv nSum `div` 2
--        y' = rv nSum - x'

----data L a b c = L (a -> b) (a -> c)
--data L a b = L a b

--primPlus_for :: Int -> Int -> Int
--primPlus_for = (+)
--primPlus_rev :: Int -> Int -> Int -> Int -> Write'
----primPlus_rev x y oZ = Writes' [Write' $ Pr (x, x'), Write' $ Pr (y, y')]
--primPlus_rev x y _oZ nZ =
--  x <-- x' <>
--  y <-- y'
--  where x' = nZ `div` 2
--        y' = nZ - x'

--(<**>) = lft_for

--lft_for :: V (a -> b) -> (V a -> V b)
--lft_for vf = \va -> V $ (rv vf) (rv va)

---- TODO: Same?
--lft_rev = lft_for

--primPlus_for0 :: V Int -> V (Int -> Int)
--primPlus_for0 = lft_for (V primPlus_for)

--primPlus_for1 :: V Int -> V Int -> V Int
--primPlus_for1 = lft_for . primPlus_for0

--primPlus_rev0 :: V Int -> V (Int -> Int -> Int -> Write')
--primPlus_rev0 = lft_rev (V primPlus_rev)

--primPlus_rev1 :: V Int -> V Int -> V (Int -> Int -> Write')
--primPlus_rev1 = lft_rev . primPlus_rev0

--(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
--f .: g = \x y -> f (g x y)

--(.::) :: (d -> e) -> (a -> b -> c -> d) -> (a -> b -> c -> e)
--f .:: g = \x y z -> f (g x y z)

--primPlus_rev2 :: V Int -> V Int -> V Int -> V (Int -> Write')
--primPlus_rev2 = lft_rev .: primPlus_rev1

---- TODO: do we want V Write'?
--primPlus_rev3 :: V Int -> V Int -> V Int -> V Int -> V Write'
----primPlus_rev3 vx vy vz vnz = (lft_rev (primPlus_rev2 vx vy vz) vnz)
--primPlus_rev3 = lft_rev .:: primPlus_rev2

--aPrimV :: V Int
--aPrimV = primPlus_for1 (V 1) (V 2)

--aPrimVRev :: V Int -> V Write'
--aPrimVRev = primPlus_rev3 (V 1) (V 2) aPrimV

--aPrimVRevWrote :: V Write'
--aPrimVRevWrote = aPrimVRev (V 30)

--aPrimV' :: V Int
--aPrimV' = (V primPlus_for) <**> (V 1) <**> (V 2)

--aPrimVRev' :: V (Int -> Write')
--aPrimVRev' = (V primPlus_rev) <**> (V 1) <**> (V 2) <**> aPrimV

--aPrimVRevWrote' :: V Write'
--aPrimVRevWrote' = aPrimVRev' <**> (V 30)

--plus :: L (V Int -> V Int -> V Int) (V Int -> V Int -> V Int -> V Int -> Write)
--plus = L plus_for plus_rev

--app :: L (V a -> b) (V a -> c) -> V a -> L b c
--app (L for rev) vx = L (for vx) (rev vx)

--foo :: L (V Int -> V Int) (V Int -> V Int -> V Int -> Write)
--foo = app plus (V 1)

---- foo'' :: V Int -> L (V Int) (V Int -> Write)
---- foo'' = app foo
---- foo' :: L (V Int) (V Int -> Write)
---- foo' =
----   let vy = V 2 
----    in case foo of L for rev -> L (for vy) (rev vy)

--foo' :: L (V Int) (V Int -> V Int -> Write)
--foo' = app foo (V (2::Int))

--foo'2 :: L (V Int) (V Int -> V Int -> Write)
--foo'2 = app (app plus (V 1)) (V (2::Int))

--rL :: L (V a) b -> a
--rL (L vx _) = rv vx

---- No: we need a new output value
--wL :: L (V a) (V a -> V a -> Write) -> V a -> Write
--wL (L oldX writer) newX = writer oldX newX

--theSum :: Int
--theSum = rL foo'2

--theWrote :: Write
--theWrote = wL foo'2 (V 20)

---- TODO
---- - lift primPlus_* as plus
----   + lift individuals
----   - maybe use (V Write') everywhere?
----   - lift them together
----   - make plus (the L) out of those plain funs
---}
