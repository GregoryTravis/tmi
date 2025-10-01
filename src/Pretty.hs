{-# LANGUAGE FlexibleInstances, InstanceSigs, TypeSynonymInstances #-}

module Pretty
( pp
, mspp ) where

import Data.List (intercalate)
import qualified Data.Map.Strict as M

import Util
import Val

mspp x = msp (pp x)

class Show a => Pretty a where
  pp :: a -> String

instance Pretty String where
  pp = id

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
  pp (Code c) = pp c
  pp (Closure env body) = paren $ spaced ["*+", pp body]
  pp (PatVar id) = id
  --pp x = show x

instance Pretty Env where
  pp GlobalLayer = "G"
  pp EmptyLayer = "[]"
  pp (MapLayer mp) = paren $ spaced (map ppsec (M.assocs mp))
    where ppsec (id, v) = id ++ ":" ++ pp v
  pp (Layers a b) = paren $ spaced [pp a, ",", pp b]

spaced :: [String] -> String
spaced xs = intercalate " " (map pp xs)

paren :: Pretty a => a -> String
paren x = "(" ++ pp x ++ ")"

bracketed :: Pretty a => a -> String
bracketed x = "{" ++ pp x ++ "}"

instance Pretty Code where
  pp (CVal x) = pp x
  pp (Id x) = x
  pp (Lam x e) = paren $ spaced ["/.", pp x, pp e]
  pp a@(App f x) = ppApp a
  pp (If b t e) = paren $ spaced ["if", pp b, "then", pp t, "else", pp e]
  pp (Case e cases) = paren $ spaced (["case", pp e, "of"] ++ map ppCase cases)
  pp (Builtin name args) = paren $ spaced ((name ++ "#") : map pp args)
  pp (Ctor name args) = paren $ spaced (name : map pp args)
  pp (CtorRec name args) = paren $ spaced (name : map ppRecEntry args)

ppApp :: Code -> String
ppApp a = paren $ spaced (map pp (gather a))
  where gather (App a@(App f x) y) = gather a ++ [y]
        gather (App f x) = [f, x]

ppRecEntry :: (Ident, Code) -> String
ppRecEntry (name, value) = paren $ spaced [name, "=", pp value]

ppCase :: (Val, Code) -> String
ppCase (v, c) = bracketed $ spaced [pp v, "->", pp c]

instance Pretty Ty where
  pp = show

consListPP :: UVal -> String
consListPP x = "[" ++ join ", " (consList2List x) ++ "]"

consList2List :: UVal -> [String]
consList2List (Cton "Cons" [x, (Val _ xs)]) = (pp x) : consList2List xs
consList2List (Cton "Nil" []) = []
consList2List (PatVar var) = [".", pp var]
consList2List x = error $ show x

join :: String -> [String] -> String
join glue vs = intercalate glue vs
