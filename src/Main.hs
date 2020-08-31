{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Util

data NC1 a b  = NC1 { nodeName :: String
                    , for :: a -> b
                    , rev :: a -> b -> a }

data N1 a b = N1 { nc1 :: NC1 a b, arg0 :: a }

type V b = NC1 W b

r :: V b -> W -> b
r (NC1 { for }) w = for w

incer :: NC1 Int Int
incer = NC1 { nodeName = "adder"
            , for = (+1)
            , rev = \_ x -> x-1 }

data W = W { anInt :: Int }
w :: W
w = W { anInt = 10 }

--_anInt :: NC1 W Int
_anInt :: V Int
_anInt = NC1 { nodeName = "_anInt"
             , for = anInt
             , rev = \w i -> w { anInt = i } }

--anIntInc :: NC1 W Int
anIntInc :: V Int
anIntInc = applyNC1 incer _anInt

applyNC1 :: NC1 b c -> NC1 a b -> NC1 a c
applyNC1 (NC1 nameBC forBC revBC) (NC1 nameAB forAB revAB) = NC1 nameAC forAC revAC
  where nameAC = nameBC ++ "+" ++ nameAB
        forAC = forBC . forAB
        -- revAC :: a -> c -> a
        revAC = \a c -> revAB a (revBC (forAB a) c)
        -- a -> b
        -- a -> b -> a
        -- b -> c -> b

main = do
  msp $ r _anInt w
  msp $ r anIntInc w
  msp "hi"
