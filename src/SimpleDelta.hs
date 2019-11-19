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

-- This isn't right because the delta that's written on the right (db) doesn't
-- produce a (WDelta db) on the left, it produces some other delta in which the
-- db is nested to arbitrary depth. This means that it should be (WDelta db')
-- which means a db' in the def, and now I think I don't need this abbreviation
-- since it's not as succinct as I had hoped.
--data Val b db = Val W (Fun W b (WDelta db) db)
--data Val b db = Val W (Fun W b (WDelta db') db)

class Delta a da where
  apply :: a -> da -> a

data ListIndexDelta a = LISet Int a deriving Show
data ListConsDelta a = ListCons a | ListSnoc a deriving Show

instance Delta [a] (ListIndexDelta a) where
  apply xs (LISet i x) = take i xs ++ [x] ++ drop (i+1) xs
instance Delta [a] (ListConsDelta a) where
  apply xs (ListCons x) = x : xs
  apply xs (ListSnoc x) = xs ++ [x]

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
        rev x xs = apply xs (LISet i x)
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

instance Delta [Int] da => Delta W (WDelta da) where
  apply w (WIntsDelta dInts) = w { ints = apply (ints w) dInts }

funWIntsI1 :: Fun W Int (WDelta (ListIndexDelta Int)) Int
funWIntsI1 = composeFunFun (arrIndex 1) funWInts

world = W
  { ints = [0, 1, 2, 3, 4]
  , strings = ["aaa", "bbb", "ccc"] }

-- This whole section was trying to work with Val
--applyFunVal :: Fun a b da db -> Val a da -> Val b db
--applyFunVal fun (Val w fun') = Val w (composeFunFun fun fun')
---- fun :: Fun a b da db
---- fun' :: Fun W a (WDelta da) da
---- (composeFunFun fun fun') :: Fun W b (WDelta db) db
---- (composeFunFun fun fun') :: Fun W b (WDelta da) db  -- actual
---- result :: Val b db

---- doesn't work
----composeFunFun :: Fun b c db dc -> Fun a b da db -> Fun a c da dc

---- Inferred type seems wrong here: WDelta db is not what it would be? Oh wait, yes it is
---- Fun a b da db -> Fun W a (WDelta da) da -> Fun W b (WDelta db) db

---- works
---- composeFunFun :: Fun a b da db -> Fun q a dq da -> Fun q b dq' db
---- works
----composeFunFun :: Fun b c db dc -> Fun a b da db -> Fun a c da' dc
--composeFunFun = undefined

simpleDeltaDemo = do
  msp $ _ints [3, 4, 5] world
  msp $ apply [4::Int, 5, 6] (LISet 1 (50::Int))
  msp $ apply (W { ints = [4::Int, 5, 6], strings = [] }) (WIntsDelta (LISet 1 (50::Int)))
  msp $ apply (W { ints = [4::Int, 5, 6], strings = [] }) (WIntsDelta (ListCons (3::Int)))
  msp $ apply (W { ints = [4::Int, 5, 6], strings = [] }) (WIntsDelta (ListSnoc (7::Int)))
  msp $ valRead world funWInts
  msp $ valRead world funWStrings
  msp $ valWrite world funWInts [40::Int, 50, 60]
  msp $ valWrite world funWStrings ["a", "b", "c"]
  msp $ valDWrite world funWInts (LISet 1 (500::Int))
  msp $ valRead world funWIntsI1
  msp $ valWrite world funWIntsI1 (1000::Int)
  msp $ valDWrite world funWIntsI1 (1000::Int)
  msp $ apply world (valDWrite world funWIntsI1 1000)
  msp "shi"
