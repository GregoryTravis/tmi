{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Internal
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
