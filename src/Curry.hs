{-# LANGUAGE ExistentialQuantification, GADTs, RecordWildCards, TypeApplications, TypeFamilies #-}

module Curry (curryMain) where

import Util

data Write = forall a. Show a => Write (R a) a
--data Write = Write
type Writes = [Write]
infix 8 <--
(<--) :: Show a => R a -> a -> [Write]
rx <-- x = [Write rx x]

instance Show Write where
  show (Write _ a) = show a

-- e.g.
-- for: a -> b -> c
-- rev: a -> R a -> b -> R b -> c -> R c

data R a = R (V (F a (a -> Writes)))

-- TODO maybe this is just V -- nope, where would the a come from!
data F f r = F f r

type family Rev a where
  Rev (a -> b) = (a -> R a -> Rev b)
  Rev a = a -> Writes

instance Show f => Show (F f r) where
  show (F f _) = show f

-- app :: F (a -> b) -> F a -> F b
-- -- app :: F (a -> b) (a -> R a -> b -> R b)
-- --     -> F a (a -> R a)
-- --     -> F b (b -> R b)
-- app (F f_for f_rev) (F a_for a_rev) = undefined

inc_for :: Int -> Int
inc_for = (+1)
inc_rev :: Int -> R Int -> Int -> Writes
inc_rev _x rx x =
  rx <-- x'
  where x' = x - 1
-- TODO maybe put Rev back in just for declaring these, maybe a type alias too
inc :: F (Int -> Int) (Rev (Int -> Int))
inc = F inc_for inc_rev

plus_for :: Int -> Int -> Int
plus_for = (+)
plus_rev :: Int -> R Int -> Int -> R Int -> Int -> Writes
plus_rev x rx y ry nZ =
  rx <-- x' <>
  ry <-- y'
  where x' = nZ `div` 2
        y' = nZ - x'

plus :: F (Int -> Int -> Int) (Rev (Int -> Int -> Int))
plus = F plus_for plus_rev
plusV = VConst plus

data W = W { anInt :: Int, anotherInt :: Int }
  deriving Show
world :: W
world = W { anInt = 1, anotherInt = 50 }

anInt_for :: W -> Int
anInt_for = anInt
anInt_rev :: W -> R W -> Int -> Writes
anInt_rev w rw nAnInt =
  rw <-- w { anInt = nAnInt }
anIntV :: V (F (W -> Int) (Rev (W -> Int)))
anIntV = VConst (F anInt_for anInt_rev)
anotherInt_for :: W -> Int
anotherInt_for = anotherInt
anotherInt_rev :: W -> R W -> Int -> Writes
anotherInt_rev w rw nanotherInt =
  rw <-- w { anotherInt = nanotherInt }
anotherIntV :: V (F (W -> Int) (Rev (W -> Int)))
anotherIntV = VConst (F anotherInt_for anotherInt_rev)
-- TODO bad name
anIntVV = VApp anIntV VRoot

-- data V a = VRoot | VConst a | forall b. VApp (V (F (b -> a))) (V (F a))
data V a where
  VRoot :: V (F W (W -> Writes))
  VConst :: F f r -> V (F f r)
  VApp :: V (F (b -> a) (Rev (b -> a))) -> V (F b (b -> Writes)) -> V (F a c)

incV :: V (F (Int -> Int) (Rev (Int -> Int)))
incV = VConst inc
threeF :: F Int (Rev Int)
threeF = F 3 undefined
threeV :: V (F Int (Rev Int))
threeV = VConst threeF
-- TODO this should be (V (F Int)), try to do that with ~?
four :: V (F Int (Rev Int))
four = VApp incV threeV
five = VApp incV four

seven :: V (F Int (Rev Int))
seven = VApp (VApp plusV threeV) four

fiftyOne = VApp (VApp plusV (VApp anotherIntV VRoot)) (VApp anIntV VRoot)

r :: V a -> a
r VRoot = (F world undefined)
r (VConst x) = x
r (VApp vf vb) =
  let (F forF _revF) = r vf
      (F forB _revB) = r vb
   in F (forF forB) (error "rev VApp not impl")

-- TOOD we want to write to anIntVV
w :: V (F a (Rev a)) -> a -> Writes
w VRoot _ = error "Can't write to VRoot"
w (VConst _) _ = error "Can't write to VConst"
w (VApp vf vb) nA =
  -- revF :: Int -> R Int -> Int -> Writes
  -- revB :: Int -> Writes
  let (F forF revF) = r vf
      (F forB revB) = r vb
   in undefined -- revF forB (R vb) nA

curryMain = do
  msp $ r threeV
  msp $ r four
  msp $ r five
  msp $ r anIntVV
  msp $ r seven
  msp $ r fiftyOne
  msp $ r (VApp incV fiftyOne)
  --msp $ length $ (case r anIntVV of Curry.F x y -> y) 100
  msp "curry hi"
