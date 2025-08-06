module Lambda
( Ident
, Lam(..)
, Env(..)
, Interp(..) ) where

import qualified Data.Map.Strict as M

import Util

type Ident = String

data Lam =
  VI Int
  VS String
  VId Ident
  Lam Ident Lam
  Closure Env Lam
  App Lam Lam
  Builtin Ident Int
  BuiltinApp Lam [Lam]
deriving Show

unVI :: Lam -> Int
unVI (VI i) = i
unVI x = error $ "Not VI: " ++ x

type Env = M.Map Ident Lam

data Interp = Interp Env Builtins

data BuiltinDef = Builtin Ident Int ([Lam] -> Lam)

data BuiltinDefs = BuiltinDefs (M.Map Ident BuiltinDef)
