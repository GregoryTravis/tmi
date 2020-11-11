{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Internal
( V
, huh
) where

import Data.Dynamic
import Data.Maybe (fromJust) 

import Util

untype :: (Typeable a, Typeable b) => (a -> b) -> (Dynamic -> Dynamic)
untype f = toDyn . f . fromJust . fromDynamic
untype2 :: (Typeable a, Typeable b, Typeable c) => (a -> b -> c) -> (Dynamic -> Dynamic -> Dynamic)
untype2 f dyx dyy = toDyn $ f (fromJust $ fromDynamic dyx) (fromJust $ fromDynamic dyy)

--type DynFun = [Dynamic] -> [Dynamic]
type D = [Dynamic]

dInfo :: D -> String
dInfo ds = show (map dynTypeRep ds)

lift_1_1 :: (Typeable a, Typeable b) => (a -> b) -> (D -> D)
lift_1_1 f [dyx] = [untype f dyx]
lift_2_1 :: (Typeable a, Typeable b, Typeable c) => (a -> b -> c) -> (D -> D)
lift_2_1 f [dyx, dyy] = [untype2 f dyx dyy]
liftRev_2_1 :: (Typeable a, Typeable b, Typeable c) => (a -> b -> c -> (a, b)) -> (D -> D -> D)
liftRev_2_1 r [dyx, dyy] [dyz] = [dyx', dyy']
  where (x', y') = r (fromJust $ fromDynamic dyx) (fromJust $ fromDynamic dyy) (fromJust $ fromDynamic dyz) 
        dyx' = toDyn x'
        dyy' = toDyn y'
liftRev_2_1 r oins outs = error (show ("umm", dInfo oins, dInfo outs))

data S =
  S { for :: D -> D
    , rev :: D -> D -> D }

-- reverse of (+)
split :: Int -> (Int, Int)
split x = (x', x'')
  where x' = x `div` 2
        x'' = x - x'
--pluss :: (Num a, Typeable a) => a -> a -> a
--pluss x y = x + y
-- TODO why can't I make this Num?
pluss :: Int -> Int -> Int
pluss = (+)
revPlus :: Int -> Int -> Int -> (Int, Int)
revPlus _ _ x = split x
liftedPlus :: D -> D
liftedPlus = lift_2_1 pluss
liftedRevPlus :: D -> D -> D
liftedRevPlus = liftRev_2_1 revPlus

anS :: S
anS = S { for = liftedPlus, rev = liftedRevPlus }

data N =
  N { n_s :: S
    , args :: D }

runNForwards :: N -> D
runNForwards (N {..}) = for n_s args

runNBackwards :: N -> D -> D
runNBackwards (N {..}) revArgs = rev n_s (for n_s args) revArgs

data V a = V N Int

r :: Typeable a => V a -> a
r (V n i) = fromJust $ fromDynamic $ (runNForwards n !! i)

applySD :: S -> D -> N
applySD s d = N { n_s = s, args = d }

anN :: N
anN = applySD anS [toDyn (4::Int), toDyn (6::Int)]

aV :: V Int
aV = V anN 0

w :: Typeable a => V a -> a -> D
w (V n@(N {..}) i) x =
  let outputs :: [Dynamic]
      outputs = upd (runNForwards n) i (toDyn x)
      origInputs :: [Dynamic]
      --origInputs = for n_s args
      origInputs = args
   in rev n_s (sfesp "origInputs" dInfo origInputs) (sfesp "origInputs" dInfo outputs)
   --in map (fromJust . fromDynamic) outputs

writ :: D
writ = w aV (24::Int)
writ' :: String
writ' = show (map dynTypeRep writ)
writ'' :: String
writ'' = show $ ((case writ of [dyx, dyy] -> (fromJust $ fromDynamic dyx, fromJust $ fromDynamic dyy)) :: (Int, Int))
avArgs :: String
avArgs = case anN of N {..} -> dInfo args
--writ' = show (length writ)
--writ' = show (dynTypeRep (head writ))
--writ' = show ((fromJust $ fromDynamic (head writ)) :: Int)
--dynTypeRep
-- writ' :: (Int, Int)
-- -- writ' = case writ of [dyx, dyy] -> (fromJust $ fromDynamic dyx, fromJust $ fromDynamic dyy)
-- writ' = case writ of [dy] -> fromJust $ fromDynamic dy

huh :: String
huh = show (r aV, writ', writ'')

{-
( S(..)
, V(..)
, huh
) where

data S i o = S { for :: i -> o, rev :: i -> o -> i }

data N i o vi {-vo-} =
  N { n_s :: S i o
    , inputs :: vi }
    --, outputs :: vo }

data V a =
  forall i o vi vo.
  V (N i o vi) (N i o vi -> a)

r :: V a -> a
r (V n getter) = getter n

anS :: S (Int, Int) Int
anS = S { for = uncurry (+), rev = rev' }
  where rev' _ x = (x', x'')
          where x' = x `div` 2
                x'' = x - x'

-- applyS_2_1 :: S (a, b) c -> (V a, V b) -> N (a, b) c (V a, V b)
-- applyS_2_1 s args = N { n_s = s, inputs = args }
applyS :: S i o -> vi -> N i o vi
applyS s args = N { n_s = s, inputs = args }

apply_1_2 :: S a (b, c) -> V a -> (V b, V c)
apply_1_2 s args =
  let n = applyS s args
      getter1 (N { n_s, inputs = va }) = fst $ for n_s (r va)
      getter2 (N { n_s, inputs = va }) = snd $ for n_s (r va)
   in (V n getter1, V n getter2)

apply_2_1 :: S (a, b) c -> (V a, V b) -> V c
apply_2_1 s args =
  let n = applyS s args
      getter (N { n_s, inputs = (va, vb) }) = for n_s (r va, r vb)
   in V n getter

constV :: a -> V a
constV x = V undefined (\_ -> x)

splitS :: S Int (Int, Int)
splitS = S { for, rev = undefined }
  where for x = (x', x'')
          where x' = x `div` 2
                x'' = x - x'

plusS :: S (Int, Int) Int
plusS = S { for = uncurry (+), rev = undefined }

mulS:: S (Int, Int) Int
mulS = S { for = uncurry (*), rev = undefined }

aSum :: V Int
aSum = apply_2_1 plusS (constV 12, constV 130)

splitAndAdd :: V Int
splitAndAdd =
  let (vi1, vi2) = apply_1_2 splitS (constV 23)
      vSum = apply_2_1 mulS (vi1, vi2)
   in vSum

huh :: Int
huh = r splitAndAdd

-- Some currying stuff

data Write = forall a. Show a => Write { showIt :: String }
data Q a b = Q (a -> ([Write], b))

infixr 0 +->
type a +-> b = Q a b

foo :: a +-> (b +-> c)
foo = undefined

{-
data N_1_2 a b c = N_1_2 { n_s :: S a (b, c)
                         , input :: V a
                         , output :: (V b, V c) }

applyS_1_2 :: S a (b, c) -> V a -> (V b, V c)
applyS_1_2 s va =
  let n = N_1_2 { n_s = s, input = va, output = (vb, vc) }
      vb = V n 0
      vc = V n 1
   in n

apply_2_1 :: S (a, b) c -> (V a, V b) -> V c
apply_2_1 s (va, vb) =
  let n = applyS_2_1 s (va, vb)
   in case n of N_2_1 { output = vc } -> vc

data N_2_1 a b c = N_2_1 { n_s :: S (a, b) c
                         , input :: (V a, V b)
                         , output :: V c }

applyS_2_1 :: S (a, b) c -> (V a, V b) -> N_2_1 a b c
applyS_2_1 s (va, vb) =
  let n = N_2_1 { n_s = s, input = (va, vb), output = vc }
      vc = V n 0
   in n

apply_2_1 :: S (a, b) c -> (V a, V b) -> V c
apply_2_1 s (va, vb) =
  let n = applyS_2_1 s (va, vb)
   in case n of N_2_1 { output = vc } -> vc

data V a = forall n. V n Int
-- data V a =
--   forall i o.
--   V {
-}

{-
( V(..)
, S(..)
--, T(..)
, N(..)
) where

-- Tuple-ish type, including a 1-tuple option
--data T a = T a | T1 a
data T0 = T0
data T1 a = T1 a
data T2 a b = T2 a b

data S i o = S { for :: i -> o, rev :: i -> o -> i }

data N i o =
  N { i :: Lifted i
    , o :: Lifted o
    , f :: S i o }
instance Show (N i o) where
  show N {..} = "N"

data V a = forall i o. V (N i o) Int
instance Show (V a) where
  show (V n i) = "V"

class Liftable a where
  type Lifted a

instance Liftable T0 where
  type Lifted T0 = ()
instance Liftable (T1 a) where
  type Lifted (T1 a) = (T1 (V a))
instance Liftable (T2 a b) where
  type Lifted (T2 a b) = (T2 (V a) (V b))

applyS :: S i o -> Lifted i -> N i o
applyS = undefined
-}
-}
