{-# LANGUAGE ExistentialQuantification, GADTs, RecordWildCards, StandaloneDeriving,
             TypeApplications, TypeFamilies #-}

module Curry (curryMain) where

import Util

data Write = forall a. Show a => Write (R a) a
--data Write = Write
type Writes = [Write]
infix 8 <--
(<--) :: Show a => R a -> a -> [Write]
rx <-- x = [Write rx x]

instance Show Write where
  show (Write r a) = "(" ++ (show r) ++ " <-- " ++ (show a) ++ ")"

-- e.g.
-- for: a -> b -> c
-- rev: a -> R a -> b -> R b -> c -> R c

data R a = R (V (B a))
  deriving Show

-- TODO maybe this is just V -- nope, where would the a come from!
data F f r = F String f r

-- TODO does this work?
-- type B a = F a (Rev a)
type B a = F a (a -> Writes)

type family Rev a where
  Rev (a -> b) = (a -> R a -> Rev b)
  Rev a = a -> Writes

instance Show (F f r) where
  show (F s f _) = show s

inc_for :: Int -> Int
inc_for = (+1)
inc_rev :: Rev (Int -> Int)
inc_rev _x rx x =
  rx <-- x'
  where x' = x - 1
-- TODO maybe put Rev back in just for declaring these, maybe a type alias too
inc :: F (Int -> Int) (Rev (Int -> Int))
inc = F "inc" inc_for inc_rev

plus_for :: Int -> Int -> Int
plus_for = (+)
plus_rev :: Rev (Int -> Int -> Int)
plus_rev x rx y ry nZ =
  rx <-- x' <>
  ry <-- y'
  where x' = nZ `div` 2
        y' = nZ - x'

plus :: F (Int -> Int -> Int) (Rev (Int -> Int -> Int))
plus = F "plus" plus_for plus_rev
plusV = VConst plus

-- plus3 :: Integer -> Float -> Double -> Double
-- plus3 xi yf zd = fromInteger xi + realToFrac yf + zd
plus3_for :: Int -> Int -> Int -> Int
plus3_for x y z = x + y + z
plus3_rev :: Rev (Int -> Int -> Int -> Int)
plus3_rev x rx y ry z rz nW =
  rx <-- x' <>
  ry <-- y' <>
  rz <-- z'
  where x' = nW `div` 3
        y' = (nW - x') `div` 2
        z' = nW - x' - y'
plus3V = VConst (F "plus3" plus3_for plus3_rev)

data W = W { anInt :: Int, anotherInt :: Int, yetAnotherInt :: Int }
  deriving Show
world :: W
world = W { anInt = 1, anotherInt = 50, yetAnotherInt = 2000 }

anInt_for :: W -> Int
anInt_for = anInt
anInt_rev :: Rev (W -> Int)
anInt_rev w rw nAnInt =
  rw <-- w { anInt = nAnInt }
anIntV :: V (F (W -> Int) (Rev (W -> Int)))
anIntV = VConst (F "anInt" anInt_for anInt_rev)
anotherInt_for :: W -> Int
anotherInt_for = anotherInt
anotherInt_rev :: Rev (W -> Int)
anotherInt_rev w rw nanotherInt =
  rw <-- w { anotherInt = nanotherInt }
anotherIntV :: V (F (W -> Int) (Rev (W -> Int)))
anotherIntV = VConst (F "anotherInt" anotherInt_for anotherInt_rev)
yetAnotherInt_for :: W -> Int
yetAnotherInt_for = yetAnotherInt
yetAnotherInt_rev :: (Rev (W -> Int))
yetAnotherInt_rev w rw nyetAnotherInt =
  rw <-- w { yetAnotherInt = nyetAnotherInt }
yetAnotherIntV = VConst (F "yetAnotherIntV" yetAnotherInt_for yetAnotherInt_rev)
-- TODO bad name
anIntVV = VApp anIntV VRoot

-- data V a = VRoot | VConst a | forall b. VApp (V (F (b -> a))) (V (F a))
data V a where
  VRoot :: V (F W (W -> Writes))
  VConst :: F f r -> V (F f r)
  VApp :: V (F (b -> a) (b -> R b -> c)) -> V (B b) -> V (F a c)

instance Show a => Show (V a) where
  show VRoot = "{root}"
  show (VConst a) = show a
  show (VApp vf vb) = "(VApp " ++ (show vf) ++ " " ++ (show vb) ++ ")"

incV :: V (F (Int -> Int) (Rev (Int -> Int)))
incV = VConst inc
threeF :: F Int (Rev Int)
threeF = F "three" 3 undefined
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
r VRoot = (F "world" world undefined)
r (VConst x) = x
r (VApp vf vb) =
  let (F sF forF revF) = r vf
      (F sx forB revB) = r vb
      -- forF :: Int -> Int
      -- revF :: Int -> R Int -> Int -> Writes
      -- forB :: Int
      -- revB :: Int -> Writes
      --   vb :: (V (F Int (Int -> Writes)))
      -- R vb :: R Int ~ R (V (F Int (Int -> Writes)))
      -- rev' :: Int -> Writes
      rev' = revF forB (R vb)
   in F (show (sF, sx)) (forF forB) rev'

-- The rev of (VApp plusV threeV)
-- arg is (VApp (VApp "plus" "three") (VApp "inc" "three"))
-- F (Int -> Int -> Int) (Int -> R Int -> Int -> R Int -> Int -> Writes)
-- VApp :: V (F (b -> a) (b -> R b -> c)) -> V (F b (b -> Writes)) -> V (F a c)

for :: F a b -> a
for (F _ f _) = f

-- TOOD we want to write to anIntVV
w :: V (B a) -> a -> Writes
w VRoot _ = error "Can't write to VRoot"
w (VConst _) _ = error "Can't write to VConst"
w (VApp vf vb) nA =
  -- revF :: Int -> R Int -> Int -> Writes
  -- revB :: Int -> Writes
  let (F _ forF revF) = r vf
      (F _ forB revB) = r vb
   in revF forB (R vb) nA

curryMain = do
  msp $ for $ r threeV
  msp $ for $ r four
  msp $ for $ r five
  msp $ for $ r anIntVV
  msp $ for $ r (VApp anotherIntV VRoot)
  msp $ for $ r (VApp yetAnotherIntV VRoot)
  msp $ for $ r seven
  msp $ for $ r fiftyOne
  msp $ for $ r (VApp incV fiftyOne)
  msp $ w four 40
  msp $ w Curry.anIntVV 99
  msp $ w (VApp Curry.anotherIntV VRoot) 700
  msp $ w seven 7001
  msp $ w (VApp (VApp plusV anIntVV) (VApp anotherIntV VRoot)) 101
  msp $ w (VApp anIntV VRoot) 50
  msp $ w (VApp anotherIntV VRoot) 51
  msp $ for $ r (VApp (VApp (VApp plus3V anIntVV) (VApp anotherIntV VRoot))
                            (VApp yetAnotherIntV VRoot))
  msp $ w (VApp (VApp (VApp plus3V anIntVV) (VApp anotherIntV VRoot))
                      (VApp yetAnotherIntV VRoot)) 25001
  msp "curry hi"
