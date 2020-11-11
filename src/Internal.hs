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

liftFor_1_1 :: (Typeable a, Typeable b) => (a -> b) -> (D -> D)
liftFor_1_1 f [dyx] = [untype f dyx]
liftRev_1_1 :: (Typeable a, Typeable b) => (a -> b -> a) -> (D -> D -> D)
liftRev_1_1 r [dyoa] [dyb] = [dynb]
  where dynb = toDyn nb
        nb = r oa b
        oa = fromJust $ fromDynamic dyoa
        b = fromJust $ fromDynamic dyb

liftFor_2_1 :: (Typeable a, Typeable b, Typeable c) => (a -> b -> c) -> (D -> D)
liftFor_2_1 f [dyx, dyy] = [untype2 f dyx dyy]
liftRev_2_1 :: (Typeable a, Typeable b, Typeable c) => (a -> b -> c -> (a, b)) -> (D -> D -> D)
liftRev_2_1 r [dyx, dyy] [dyz] = [dyx', dyy']
  where (x', y') = r (fromJust $ fromDynamic dyx) (fromJust $ fromDynamic dyy) (fromJust $ fromDynamic dyz) 
        dyx' = toDyn x'
        dyy' = toDyn y'
--liftRev_2_1 r oins outs = error (show ("umm", dInfo oins, dInfo outs))

-- TODO these two lifts are nearly the same
lift_1_1 :: (Typeable a, Typeable b) => F a b -> S
lift_1_1 (F {..}) = S { for, rev }
  where for = liftFor_1_1 ffor
        rev = liftRev_1_1 frev

lift_2_1 :: (Typeable a, Typeable b, Typeable c) => F2 a b c -> S
lift_2_1 (F2 {..}) = S { for, rev }
  where for = liftFor_2_1 ffor2
        rev = liftRev_2_1 frev2

data F a b = F { ffor :: a -> b, frev :: a -> b -> a }
data F2 a b c = F2 { ffor2 :: a -> b -> c, frev2 :: a -> b -> c -> (a, b) }

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
liftedPlus = liftFor_2_1 pluss
liftedRevPlus :: D -> D -> D
liftedRevPlus = liftRev_2_1 revPlus
plusF :: F2 Int Int Int
plusF = F2 { ffor2 = pluss, frev2 = revPlus }

anS :: S
--anS = S { for = liftedPlus, rev = liftedRevPlus }
anS = lift_2_1 plusF

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
