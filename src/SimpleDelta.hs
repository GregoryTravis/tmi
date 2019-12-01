{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}

module SimpleDelta
( simpleDeltaDemo
) where

import qualified Data.Map.Strict as M
import qualified Debug.Trace as TR

import Util 

data Fun a b da db = Fun
  { for :: a -> b
  , rev :: b -> a -> a
  , drev :: db -> a -> da }

composeFunFun :: Fun b c db dc -> Fun a b da db -> Fun a c da dc
composeFunFun (Fun { for = forBC, rev = revBC, drev = drevBC }) (Fun { for = forAB, rev = revAB, drev = drevAB }) = Fun { for = forAC, rev = revAC, drev = drevAC }
  where forAC = forBC . forAB
        -- revAC :: c -> a -> a
        revAC c a = revAB b a
          where b = revBC c b'
                b' = forAB a
        -- drevAC :: dc -> a -> da
        drevAC dc a = drevAB db a
          where db = drevBC dc b
                b = forAB a

data ListIndexDelta a = LISet Int a deriving Show
data ListConsDelta a = ListCons a | ListSnoc a deriving Show

data W = W
  { ints :: [Int] 
  , strings :: [String] }
  deriving (Eq, Show)
_ints :: [Int] -> W -> W
_ints xs w = w { ints = xs }
_strings ss w = w { strings = ss }

data WDelta da = WIntsDelta da | WStringsDelta da deriving Show
_d_ints :: db -> a -> WDelta db
_d_ints dInts _ = WIntsDelta dInts
_d_strings dStrings _ = WStringsDelta dStrings

-- dlens of (!!)
arrIndex :: Int -> Fun [a] a (ListIndexDelta da) da
arrIndex i = Fun { for, rev, drev }
  where for = (!!i)
        rev x xs = take i xs ++ [x] ++ drop (i+1) xs
        drev dx _ = LISet i dx

funWInts :: Fun W [Int] (WDelta di) di
funWInts = Fun { for, rev, drev }
  where for = ints
        rev = _ints
        drev = _d_ints

funWStrings :: Fun W [String] (WDelta di) di
funWStrings = Fun { for, rev, drev }
  where for = strings
        rev = _strings
        drev = _d_strings

valRead :: W -> Fun W a (WDelta di) da -> a
valRead world (Fun { for }) = for world

valWrite :: W -> Fun W a (WDelta di) da -> a -> W
valWrite world (Fun { rev }) a = rev a world

valDWrite :: W -> Fun W a (WDelta di) da -> da -> (WDelta di)
valDWrite world (Fun { drev }) da = drev da world

-- This is just the identity dlens
-- funW :: Fun W W (WDelta di) (WDelta di)
funW :: Fun a a da da
funW = Fun { for, rev, drev }
  where for = id
        rev = \w _ -> w
        drev = \dw _ -> dw

-- Probably pointless
funWInts' = composeFunFun funWInts funW

funWIntsI1 :: Fun W Int (WDelta (ListIndexDelta Int)) Int
funWIntsI1 = composeFunFun (arrIndex 1) funWInts

world = W
  { ints = [0, 1, 2, 3, 4]
  , strings = ["aaa", "bbb", "ccc"] }

simpleDeltaDemo = do
  msp $ _ints [3, 4, 5] world
  -- msp $ apply [4::Int, 5, 6] (LISet 1 (50::Int))
  -- msp $ apply (W { ints = [4::Int, 5, 6], strings = [] }) (WIntsDelta (LISet 1 (50::Int)))
  -- msp $ apply (W { ints = [4::Int, 5, 6], strings = [] }) (WIntsDelta (ListCons (3::Int)))
  -- msp $ apply (W { ints = [4::Int, 5, 6], strings = [] }) (WIntsDelta (ListSnoc (7::Int)))
  msp $ valRead world funWInts
  msp $ valRead world funWStrings
  msp $ valWrite world funWInts [40::Int, 50, 60]
  msp $ valWrite world funWStrings ["a", "b", "c"]
  msp $ valDWrite world funWInts (LISet 1 (500::Int))
  msp $ valRead world funWIntsI1
  msp $ valWrite world funWIntsI1 (1000::Int)
  msp $ valDWrite world funWIntsI1 (1000::Int)
  -- msp $ apply world (valDWrite world funWIntsI1 1000)
  msp "shi"
