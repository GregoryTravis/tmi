{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}
--{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Internal
( V
, huh
) where

import Data.Dynamic
--import Data.Maybe (fromJust) 

import Util

dy :: Typeable a => a -> Dynamic
dy = toDyn
undy :: Typeable a => Dynamic -> a
undy dyn = case fromDynamic dyn of Just x -> x
                                   Nothing -> error msg
  where msg = "Can't convert " ++ (show (dynTypeRep dyn)) ++ " to a"

untype :: (Typeable a, Typeable b) => (a -> b) -> (Dynamic -> Dynamic)
untype f = dy . f . undy
untype2 :: (Typeable a, Typeable b, Typeable c) => (a -> b -> c) -> (Dynamic -> Dynamic -> Dynamic)
untype2 f dyx dyy = dy $ f (undy dyx) (undy dyy)

--type DynFun = [Dynamic] -> [Dynamic]
type D = [Dynamic]

dInfo :: D -> String
dInfo ds = show (map dynTypeRep ds)

liftFor_1_1 :: (Typeable a, Typeable b) => (a -> b) -> (D -> D)
--liftFor_1_1 f [dyx] = [untype f dyx]
liftFor_1_1 f [dyva] = [dy (f (r (undy dyva)))]
liftRev_1_1 :: (Typeable a, Typeable b) => (a -> b -> a) -> (D -> D -> D)
liftRev_1_1 rev [dyoa] [dyb] = [dy (rev (r (undy dyoa)) (undy dyb))]
liftFor_2_1 :: (Typeable a, Typeable b, Typeable c) => (a -> b -> c) -> (D -> D)
liftFor_2_1 f [dyx, dyy] = [dy (f (r (undy dyx)) (r (undy dyy)))]
liftRev_2_1 :: (Typeable a, Typeable b, Typeable c) => (a -> b -> c -> (a, b)) -> (D -> D -> D)
liftRev_2_1 rev [dyx, dyy] [dyz] = [dyx', dyy']
  where (x', y') = rev (r (undy dyx)) (r (undy dyy)) (undy dyz) 
        dyx' = dy x'
        dyy' = dy y'
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
-- liftedPlus :: D -> D
-- liftedPlus = liftFor_2_1 pluss
-- liftedRevPlus :: D -> D -> D
-- liftedRevPlus = liftRev_2_1 revPlus
plusF :: F2 Int Int Int
plusF = F2 { ffor2 = pluss, frev2 = revPlus }

anS :: S
--anS = S { for = liftedPlus, rev = liftedRevPlus }
anS = lift_2_1 plusF

data N =
  N { n_s :: S
    , args :: D }

-- Constant: like () -> a
-- TODO: use a lifter here?
konstS :: Typeable a => a -> S
konstS x = S {..}
  where for [] = [dy x]
        rev _ _ = error "Constant: no reverse"

konstN :: Typeable a => a -> N
konstN x = N {..}
  where n_s = konstS x
        args = []

konstV :: Typeable a => a -> V a
konstV x = V n 0
  where n = konstN x

-- dynMap :: (Typeable a, Typeable b) => (forall a b. a -> b) -> D -> D
-- dynMap f = map (untype f)

runNForwards :: N -> D
runNForwards (N {..}) = for n_s args -- (dynMap r args)

runNBackwards :: N -> D -> D
runNBackwards (N {..}) revArgs = rev n_s (for n_s args) revArgs

data V a = V N Int
  -- deriving Typeable

r :: Typeable a => V a -> a
r (V n i) = undy $ (runNForwards n !! i)

applySD :: S -> D -> N
applySD s d = N { n_s = s, args = d }

-- Now we want a typed wrapper created from the typed primitive.
-- This should be the only interface to this stuff, since it enforces type
-- safety around the dynamic stuff.
hoist2 :: (Typeable a, Typeable b, Typeable c) => F2 a b c -> (V a -> V b -> V c)
hoist2 f va vb =
  let s :: S
      s = lift_2_1 f
      d :: D
      d = [dy va, dy vb]
      n :: N
      n = applySD s d
      v = V n 0
   in v

anN :: N
--anN = applySD anS [dy 4::Int), dy (6::Int)]
anN = applySD anS [dy (konstV (4::Int)), dy (konstV (6::Int))]

aV :: V Int
aV = V anN 0

aV' :: V Int
aV' = hoist2 plusF (konstV (40::Int)) (konstV (60::Int))
-- Nope, type error (good)
-- aV'' = hoist2 plusF (konstV (40::Int)) (konstV ("aaa"::String))

w :: Typeable a => V a -> a -> D
w (V n@(N {..}) i) x =
  let outputs :: [Dynamic]
      outputs = upd (runNForwards n) i (dy x)
      origInputs :: [Dynamic]
      --origInputs = for n_s args
      origInputs = args
   in rev n_s (sfesp "origInputs" dInfo origInputs) (sfesp "origInputs" dInfo outputs)
   --in map (undy) outputs

writ :: D
writ = w aV 24
writ' :: String
writ' = show (map dynTypeRep writ)
writ'' :: String
writ'' = show $ ((case writ of [dyx, dyy] -> (undy dyx, undy dyy)) :: (Int, Int))
writ''' :: String
writ''' = show $ ((case w aV' 260 of [dyx, dyy] -> (undy dyx, undy dyy)) :: (Int, Int))
avArgs :: String
avArgs = case anN of N {..} -> dInfo args
--writ' = show (length writ)
--writ' = show (dynTypeRep (head writ))
--writ' = show ((undy (head writ)) :: Int)
--dynTypeRep
-- writ' :: (Int, Int)
-- -- writ' = case writ of [dyx, dyy] -> (undy dyx, undy dyy)
-- writ' = case writ of [dy] -> undy dy

huh :: String
huh = show (r aV, r aV', writ', writ'', writ''')

-- Some currying stuff

data Write = forall a. Show a => Write { showIt :: String }
data Q a b = Q (a -> ([Write], b))

infixr 0 +->
type a +-> b = Q a b

foo :: a +-> (b +-> c)
foo = undefined

data InOut a b = In (a -> b) | Out a b

-- doInOut (In f) x = f x
-- doInOut (Out a b) f
