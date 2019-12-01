{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
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

data W = W
  { ints :: [Int] 
  , strings :: [String] }
  deriving (Eq, Show)
_ints :: [Int] -> W -> W
_ints xs w = w { ints = xs }
_strings ss w = w { strings = ss }

_d_ints :: db -> a -> WIntsDelta db
_d_ints dInts _ = WIntsDelta dInts
_d_strings dStrings _ = WStringsDelta dStrings

-- dlens of (!!)
arrIndex :: Int -> Fun [a] a (ListIndexDelta da) da
arrIndex i = Fun { for, rev, drev }
  where for = (!!i)
        rev x xs = take i xs ++ [x] ++ drop (i+1) xs
        drev dx _ = Update i dx

-- But which allows nested deltas
--arrIndex :: Int -> Fun [a] a (ListIndexDelta da) da
arrIndex' i = Fun { for, rev, drev }
  where for = (!!i)
        rev x xs = take i xs ++ [x] ++ drop (i+1) xs
        drev dx _ = Update' i dx

funWInts :: Fun W [Int] (WIntsDelta di) di
funWInts = Fun { for, rev, drev }
  where for = ints
        rev = _ints
        drev = _d_ints

funWStrings :: Fun W [String] (WStringsDelta di) di
funWStrings = Fun { for, rev, drev }
  where for = strings
        rev = _strings
        drev = _d_strings

valRead :: W -> Fun W a dw da -> a
valRead world (Fun { for }) = for world

valWrite :: W -> Fun W a dw da -> a -> W
valWrite world (Fun { rev }) a = rev a world

valDWrite :: W -> Fun W a dw da -> da -> dw
valDWrite world (Fun { drev }) da = drev da world

-- This is just the identity dlens
funW :: Fun a a da da
funW = Fun { for, rev, drev }
  where for = id
        rev = \w _ -> w
        drev = \dw _ -> dw

-- Probably pointless
funWInts' = composeFunFun funWInts funW

funWIntsI1 :: Fun W Int (WIntsDelta (ListIndexDelta Int)) Int
funWIntsI1 = composeFunFun (arrIndex 1) funWInts

funWStringsI i = composeFunFun (arrIndex i) funWStrings
funWStringsI' i = composeFunFun (arrIndex' i) funWStrings

world = W
  { ints = [0, 1, 2, 3, 4]
  , strings = ["aaa", "bbb", "ccc"] }

data ListIndexDelta a = Insert Int a | Delete Int | Update Int a deriving Show

data ListConsDelta a = Cons a | Snoc a deriving Show

class Delta d where
  type V d
  apply :: V d -> d -> V d

instance Delta (ListIndexDelta a) where
  type V (ListIndexDelta a) = [a]
  apply xs (Insert i x) = take i xs ++ [x] ++ drop i xs
  apply xs (Update i x) = take i xs ++ [x] ++ drop (i+1) xs
  apply xs (Delete i) = take i xs ++ drop (i+1) xs

instance Delta (ListConsDelta a) where
  type V (ListConsDelta a) = [a]
  apply xs (Cons x) = x:xs
  apply xs (Snoc x) = xs ++ [x]

data StringDelta = Prepend String | Append String deriving Show

instance Delta StringDelta where
  type V StringDelta = String
  apply s (Prepend s') = s' ++ s
  apply s (Append s') = s ++ s'

-- Goal: modify an array of strings: change the second element, and change it by prepending to it
-- s: string
-- ds: StringDelta
data Foo s ds = Update' Int ds deriving Show

instance Delta ds => Delta (Foo s ds) where
  type V (Foo s ds) = [V ds] -- crucial
  apply ss (Update' i ds) = apply ss (Update i newS)
    where newS = apply (ss !! i) ds

data FullDelta a = FullDelta a

instance Delta (FullDelta a) where
  type V (FullDelta a) = a
  apply x (FullDelta dx) = dx

-- Definite separate types for each field, since the argument type cannot be the same
data WIntsDelta da = WIntsDelta da deriving Show
data WStringsDelta da = WStringsDelta da deriving Show

-- The problem here is that there is no way to say that the V for da is definitely [Int].
-- I can't state what da is, since it could be either ListIndexDelta or ListConsDelta, and
-- for both of those, V is [Int], but I don't think I can assert that. Oh wait, I can:
instance (Delta da, (V da) ~ [Int]) => Delta (WIntsDelta da) where
  type V (WIntsDelta da) = W
  apply world (WIntsDelta da) = world { ints = apply (ints world) da }
instance (Delta da, (V da) ~ [String]) => Delta (WStringsDelta da) where
  type V (WStringsDelta da) = W
  apply world (WStringsDelta da) = world { strings = apply (strings world) da }

simpleDeltaDemo = do
  msp $ _ints [3, 4, 5] world
  msp $ apply [4, 5, 6] (Update 1 50)
  msp $ apply (W { ints = [4, 5, 6], strings = [] }) (WIntsDelta (Update 1 50))
  msp $ apply (W { ints = [4, 5, 6], strings = [] }) (WIntsDelta (Cons 3))
  msp $ apply (W { ints = [4, 5, 6], strings = [] }) (WIntsDelta (Snoc 7))
  msp $ apply world (WStringsDelta (Snoc "sn"))
  msp $ valRead world funWInts
  msp $ valRead world funWStrings
  msp $ valWrite world funWInts [40, 50, 60]
  msp $ valWrite world funWStrings ["a", "b", "c"]
  msp $ valDWrite world funWInts (Update 1 500)
  msp $ valRead world funWIntsI1
  msp $ valWrite world funWIntsI1 1000
  msp "hhi"
  msp $ valDWrite world funWIntsI1 1000
  msp $ apply world (valDWrite world funWIntsI1 1000)
  msp $ apply ["one", "two", "three"] (Update' 1 (Prepend "sh"))
  msp $ apply ["asdf", "zxcv", "qwer"] (Update' 1 (Append "aa"))
  msp $ apply [["a", "b"], ["asdf", "zxcv", "qwer"]] (Update' 1 (Update' 1 (Append "aa")))
  msp $ apply ["asdf", "zxcv", "qwer"] (Update' 1 (FullDelta "zzzz"))
  msp $ apply [["a", "b"], ["asdf", "zxcv", "qwer"]] (Update' 1 (Update' 1 (FullDelta "xxxx")))
  msp $ apply world (WStringsDelta (Update' 1 (Append "yy")))
  msp $ apply world (WStringsDelta (Update' 1 (FullDelta "yy")))
  msp $ valRead world (funWStringsI 0)
  msp $ valRead world (funWStringsI 1)
  -- prepend string to world strings 1
  msp $ valDWrite world (funWStringsI 1) (Append "qq")
  msp $ valDWrite world (funWStringsI' 1) (Append "qq")
  msp $ apply world (valDWrite world (funWStringsI' 1) (Prepend "qq"))
  -- append string to world strings 1
  msp $ apply world (valDWrite world (funWStringsI' 2) (Append "qq"))
  msp "shi"
