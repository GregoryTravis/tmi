module Value
( Value(..)
, Code(..)
, Env
, Typ(..)
, TCtor(..)
, typOf
, checkSameType
, checkSameTypes
, trecToTrel )where

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Name
import Util

type Env = M.Map Name Value

data Code =
    Prim Name
  | Lam Name Code
  | Clo Env Name Code
  | PrimLam Name [Name]
  | Const Value
  | App Code Code
  | Var Name
  | If Code Code Code
  deriving (Eq, Show, Ord)

data Value =
    I Int
  | S String
  | B Bool
  | Rel Typ (S.Set Value)
  | Rec Typ (M.Map Name Value)
  | Lst Typ [Value]
  | Tuple Typ [Value]
  | Adt Typ Name [Value]
  | Fun Typ Code
  deriving (Eq, Show, Ord)

data TCtor = TCtor Name [Typ]
  deriving (Eq, Show, Ord)

data Typ =
    TI
  | TS
  | TB
  | TRel (M.Map Name Typ)
  | TRec (M.Map Name Typ)
  | TLst Typ
  | TTuple [Typ]
  | TAdt Name [TCtor]
  | TFun -- Typ Typ
  deriving (Eq, Show, Ord)

typOf :: Value -> Typ
typOf (I _) = TI
typOf (S _) = TS
typOf (B _) = TB
typOf (Rel typ _) = typ
typOf (Rec typ _) = typ
typOf (Lst typ _) = typ
typOf (Tuple typ _) = typ
typOf (Adt typ _ _) = typ
typOf (Fun typ _) = typ

trecToTrel (TRec m) = (TRel m)

checkSameType :: Typ -> Typ -> a -> a
checkSameType xt yt = assertM "type mismatch" ok
  where ok = xt == yt

checkSameTypes :: [Typ] -> a -> a
checkSameTypes tys | null tys = error "checkSameTypes must take a non-empty list"
                   | otherwise = assertM "checkSameTypes" (go (head tys) (tail tys))
  where go ty [] = True
        go ty (ty0:tys) = (ty == ty0) && go ty tys

