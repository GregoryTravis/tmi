{-# LANGUAGE InstanceSigs #-}

module Pretty
( pp
, mspp ) where

import Data.List (intercalate)

import Util
import Val

mspp x = msp (pp x)

class Show a => Pretty a where
  pp :: a -> String

instance Pretty Val where
  pp :: Val -> String
  pp (Val DK x) = pp x
  pp (Val ty x) = "(" ++ pp x ++ " : " ++ pp ty ++ ")"

instance Pretty UVal where
  pp (VI i) = show i
  pp (VB b) = show b
  pp (VS s) = show s
  pp x@(Cton "Cons" _) = consListPP x
  pp x@(Cton "Nil" _) = consListPP x
  pp x = show x

instance Pretty Code where
  pp (CVal x) = pp x
  pp x = show x

instance Pretty Ty where
  pp = show

consListPP :: UVal -> String
consListPP x = "[" ++ joinPP ", " (consList2List x) ++ "]"

consList2List :: UVal -> [Val]
consList2List (Cton "Cons" [x, (Val _ xs)]) = x : consList2List xs
consList2List (Cton "Nil" []) = []
consList2List x = error $ show x

joinPP :: String -> [Val] -> String
joinPP glue vs = intercalate glue (map pp vs)
