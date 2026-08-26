module Val
( Ident
, Val(..)
, UVal(..)
, Code(..)
, Ty(..)
, TyCtor(..)
, unCVal
, unVI
, dkv
, kI
, kB
, kS
, ckI
, ckB
, ckS
, Interp(..)
, Outside(..)
, BuiltinDef(..)
, BuiltinDefs(..)
, History(..)
, Env(..) ) where

import qualified Data.Map.Strict as M

import Util

type Ident = String

-- TODO one day this should be a user-level ADT
data Code =
    Id Ident
  | Lam Ident Code
  | App Code Code
  | If Code Code Code
  | Case Code [(Val, Code)]
  | Builtin Ident [Code]
  | Ctor Ident [Code]
  | CtorRec Ident [(Ident, Code)]
  | CVal Val
  deriving (Eq, Show)

unCVal (CVal x) = x

data UVal =
    VI Int
  | VS String
  | VB Bool
  | Cton Ident [Val] | CtonRec Ident [(Ident, Val)]
  | PatVar Ident
  | Underscore
  | Code Code
  | Closure Env Code
  deriving (Eq, Show)

data Ty =
    TI
  | TS
  | TB
  | TFun Ty Ty
  | TAdt Ident [TyCtor]
  | DK
  deriving (Eq, Show)

data TyCtor = TyCtor Ident [Ty] | TyCtorRec Ident [(Ident, Ty)]
  deriving (Eq, Show)

data Val = Val Ty UVal
  deriving (Eq, Show)

unVI :: Val -> Int
unVI (Val _ (VI i)) = i
unVI x = error $ "Not VI: " ++ show x

dkv :: UVal -> Val
dkv x = Val DK x

kI :: Int -> Val
kI = dkv . VI

kB :: Bool -> Val
kB = dkv . VB

kS :: String -> Val
kS = dkv . VS

ckI :: Int -> Code
ckI = CVal . dkv . VI

ckB :: Bool -> Code
ckB = CVal . dkv . VB

ckS :: String -> Code
ckS = CVal . dkv . VS

data Interp = Interp History Outside
  deriving Show

data BuiltinDef = BuiltinDef Ident Int ([Val] -> Val)

instance Show BuiltinDef where
  show (BuiltinDef name arity _) = "#<builtin " ++ name ++ " " ++ show arity ++ ">"

data BuiltinDefs = BuiltinDefs (M.Map Ident BuiltinDef)
  deriving Show

data History = History [Val]
  deriving Show

data Outside = Outside {
    builtinDefs :: BuiltinDefs
  }
  deriving Show

data Env = MapLayer (M.Map Ident Val) | GlobalLayer | Layers Env Env | EmptyLayer
  deriving (Eq, Show)
