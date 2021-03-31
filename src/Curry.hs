{-# LANGUAGE ExistentialQuantification, GADTs, RecordWildCards, TypeApplications, TypeFamilies #-}

module Curry (curryMain) where

import Util

data Write = forall a. Write (R a) a
--data Write = Write
type Writes = [Write]
infix 8 <--
(<--) :: Show a => R a -> a -> [Write]
rx <-- x = [Write rx x]

-- e.g.
-- for: a -> b -> c
-- rev: a -> R a -> b -> R b -> c -> R c

data R a = R (a -> Writes)
-- TODO only created this Q so I could show the result of a rev could be different
data Q a = Q (a -> Writes)

-- TODO maybe this is just V -- nope, where would the a come from!
data F a = F a (Rev a)

instance Show a => Show (F a) where
  show (F a _) = show a

app :: F (a -> b) -> F a -> F b
-- app :: F (a -> b) (a -> R a -> b -> R b)
--     -> F a (a -> R a)
--     -> F b (b -> R b)
app (F f_for f_rev) (F a_for a_rev) = undefined

type family Rev a where
  Rev (a -> b) = (a -> R a -> Rev b)
  Rev a = a -> Q a

inc_for :: Int -> Int
inc_for = (+1)
inc_rev :: Int -> R Int -> Int -> Q Int
inc_rev _x rx x = Q $ \_ ->
  rx <-- x'
  where x' = x - 1
inc :: F (Int -> Int)
inc = F inc_for inc_rev

plus_for :: Int -> Int -> Int
plus_for = (+)
plus_rev :: Int -> R Int -> Int -> R Int -> Int -> Q Int
plus_rev x rx y ry nZ = Q $ \_ ->
  rx <-- x' <>
  ry <-- y'
  where x' = nZ `div` 2
        y' = nZ - x'

plus :: F (Int -> Int -> Int)
plus = F plus_for plus_rev
plusV = VConst plus

data W = W { anInt :: Int }
  deriving Show
world :: W
world = W { anInt = 1 }

anInt_for :: W -> Int
anInt_for = anInt
anInt_rev :: W -> R W -> Int -> Q Int
anInt_rev w rw nAnInt = Q $ \_ ->
  rw <-- w { anInt = nAnInt }
anIntV :: V (F (W -> Int))
anIntV = VConst (F anInt_for anInt_rev)
-- TODO bad name
anIntVV = VApp anIntV VRoot

-- data V a = VRoot | VConst a | forall b. VApp (V (F (b -> a))) (V (F a))
data V a where
  VRoot :: V (F W)
  VConst :: F a -> V (F a)
  VApp :: (V (F (b -> a))) -> (V (F b)) -> V (F a) -- a

incV :: V (F (Int -> Int))
incV = VConst inc
threeF :: F Int
threeF = F 3 undefined
threeV :: V (F Int)
threeV = VConst threeF
-- TODO this should be (V (F Int)), try to do that with ~?
four :: V (F Int)
four = VApp incV threeV
five = VApp incV four

seven :: V (F Int)
seven = VApp (VApp plusV threeV) four

r :: V a -> a
r VRoot = (F world undefined)
r (VConst x) = x
r (VApp vf vb) =
  let (F forF revF) = r vf
      (F forB revB) = r vb
   in F (forF forB) undefined

curryMain = do
  msp $ r threeV
  msp $ r four
  msp $ r five
  msp $ r anIntVV
  msp $ r seven
  msp "curry hi"
