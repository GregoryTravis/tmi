{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Internal
( V(..)
, S(..)
, T(..)
, N(..)
) where

-- Tuple-ish type, including a 1-tuple option
data T a = T a | T1 a

data S i o = S { for :: i -> o, rev :: i -> o -> i }

data N =
  forall i o.
  N { i :: i
    , o :: o
    , f :: S i o }
instance Show N where
  show N {..} = "n"

data V a = V N Int
  deriving Show

--type a +-> b
