{-# LANGUAGE ExistentialQuantification, RecordWildCards #-}

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
