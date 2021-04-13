{-# LANGUAGE ExistentialQuantification, GADTs, RecordWildCards, StandaloneDeriving,
             TypeApplications, TypeFamilies #-}

module Curry (curryMain) where

import Control.Applicative

import Util

data W = W

data F a = F (W -> a)
data R a = R (W -> a -> W)

data V a = V (F a) (R a)

instance Functor V where
  fmap = undefined

instance Applicative V where
  pure = undefined
  (<*>) = undefined
  --liftA2 :: (a -> b -> c) -> f a -> f b -> f c

inc :: V (Int -> Int)
inc = undefined

lInc :: V Int -> V Int
lInc = (inc <*>)

plus :: V (Int -> Int -> Int)
plus = undefined

plusV :: V Int -> V Int -> V Int
plusV = (<*>) . (plus <*>)

-- Combining form?

data NamedFun f = NamedFun String f

instance Show (NamedFun f) where
  show (NamedFun name _) = name

data Combi c =
  forall a b. (Show a, Show b) =>
    Combi (NamedFun (a -> b -> c)) (Combi a) (Combi b) |
  ConstCombi c
 
-- deriving instance Show c => Show (Combi c)

instance Show c => Show (Combi c) where
  show (ConstCombi c) = show c
  show (Combi (NamedFun name _) ca cb) = "(" ++ name ++ " " ++ (show ca) ++ " " ++ (show cb) ++ ")"

readCombi :: Combi c -> c
readCombi (Combi (NamedFun _ f) ca cb) = f (readCombi ca) (readCombi cb)
readCombi (ConstCombi c) = c

nplus = NamedFun "plus" (+)

c :: Combi Int
c = Combi nplus (ConstCombi 1)(ConstCombi 2)
c2 = Combi nplus c c

($$) :: NamedFun (a -> b) -> a -> b
NamedFun _ f $$ x = f x

d :: Combi Int
d = Combi (NamedFun "$$" ($$)) (ConstCombi (NamedFun "(+1)" (+1))) (ConstCombi 10)

curryMain = do
  msp $ readCombi c
  msp c
  msp $ readCombi d
  msp d
  -- Causes panic
  -- msp $ readCombi c2
  -- msp c2
  msp "curry hi"
