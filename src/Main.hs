{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Util

data NC1 a b  = NC1 { nodeName :: String
                    , for :: a -> b
                    , rev :: a -> b -> a }

data N1 a b = N1 { nc1 :: NC1 a b, arg0 :: a }

incer :: NC1 Int Int
incer = NC1 { nodeName = "adder"
            , for = (+1)
            , rev = \_ x -> x-1 }

data W = W { anInt :: Int }
w :: W
w = W { anInt = 10 }

_anInt :: NC1 W Int
_anInt = NC1 { nodeName = "_anInt"
             , for = anInt
             , rev = \w i -> w { anInt = i } }

type V b = NC1 W b

r :: V b -> W -> b
r (NC1 { for }) w = for w

main = do
  msp $ r _anInt w
  msp "hi"
