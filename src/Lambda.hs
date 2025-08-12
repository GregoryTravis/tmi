module Lambda
( Ident
, Lam(..)
, unVI
, Env(..)
, Interp(..)
, BuiltinDef(..)
, BuiltinDefs(..) ) where

import qualified Data.Map.Strict as M

import Util

type Ident = String

data Lam =
    VI Int
  | VS String
  | VId Ident
  | Lam Ident Lam
  | Closure Env Lam
  | App Lam Lam
  | Builtin Ident Int
  | BuiltinApp Lam [Lam]
  deriving Show

unVI :: Lam -> Int
unVI (VI i) = i
unVI x = error $ "Not VI: " ++ show x

data Env = Env (M.Map Ident Lam)
  deriving Show

data Interp = Interp Env BuiltinDefs

data BuiltinDef = BuiltinDef Ident Int ([Lam] -> Lam)

data BuiltinDefs = BuiltinDefs (M.Map Ident BuiltinDef)
