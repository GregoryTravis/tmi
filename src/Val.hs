module Val
( Ident
, Val(..)
, UVal(..)
, Code(..)
, Ty(..)
, unCVal
, unVI
, dkv
, kI
, kB
, kS
, Env(..)
, Interp(..)
, BuiltinDef(..)
, BuiltinDefs(..)
, app2 ) where

import qualified Data.Map.Strict as M

import Util

type Ident = String

-- TODO one day this should be a user-level ADT
data Code =
    Id Ident
  | Lam Ident Code
  | App Code Code
  | If Code Code Code
  | Builtin Ident Int
  | BuiltinApp Code [Code]
  | CVal Val
  deriving (Eq, Show)

unCVal (CVal x) = x

data UVal =
    VI Int
  | VS String
  | VB Bool
  | Code Code
  | Closure Env Code
  deriving (Eq, Show)

data Ty =
    TI
  | TS
  | TB
  | TFun Ty Ty
  | TCton Ident [Ty]
  | DK
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

data Env = Env (M.Map Ident Val)
  deriving (Eq, Show)

data Interp = Interp Env BuiltinDefs

data BuiltinDef = BuiltinDef Ident Int ([Val] -> Val)

data BuiltinDefs = BuiltinDefs (M.Map Ident BuiltinDef)

app2 :: Code -> Code -> Code -> Code
app2 f a b = CVal $ Val DK (Code (App (App f a) b))
