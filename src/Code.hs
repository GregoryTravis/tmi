module Code
( eval ) where

import Display
import Name
import Rec
import Rel
import Tuple
import Util
import Value

import Prelude hiding (lookup)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

data PrimDef = PrimDef Name Integer
  deriving (Eq, Show, Ord)

primDefs = [
    (PrimDef "==" 2)
  , (PrimDef "+1" 1)
  , (PrimDef "-" 2)
  , (PrimDef "*" 2)
  ]

primDefToLam :: PrimDef -> Code
primDefToLam (PrimDef name arity) =
  let go [] = (PrimLam name vars)
      go (var : vars) = Lam var (go vars)
  in go vars
  where vars = map toVar [0..arity-1]
        toVar i = "x" ++ show i

primLams :: M.Map Name Code
primLams = M.fromList (map entry primDefs)
  where entry x@(PrimDef name _) = (name, primDefToLam x)

lookupPrimLam :: Name -> Code
lookupPrimLam name = case M.lookup name primLams of
    Just x -> x
    Nothing -> error ("Prim " ++ name ++ " not found")

apply :: Env -> Value -> Value -> Value
apply genv (Fun TFun (Clo env var body)) arg =
  eval genv (extend env var arg) body

extend :: Env -> Name -> Value -> Env
extend env name value = M.insert name value env

-- Surely this exists already.
whatsThis :: Maybe a -> Maybe a -> Maybe a
whatsThis (Just x) _ = Just x
whatsThis Nothing x = x

twoMapLookup :: Env -> Env -> Name -> Maybe Value
twoMapLookup genv env name = whatsThis (M.lookup name env) (M.lookup name genv)

lookup :: Env -> Env -> Name -> Value
lookup genv env name =
  case twoMapLookup genv env name of
    Just x -> x
    Nothing -> error ("Undefined " ++ name ++ " in " ++ show env)

eval :: Env -> Env -> Code -> Value
eval genv env (Prim name) = eval genv env (lookupPrimLam name)
eval genv env (Lam name code) = Fun TFun (Clo env name code)
eval genv env (App fun arg) =
  apply genv (eval genv env fun) (eval genv env arg)
eval genv env (Const value) = value
eval genv env (Var name) = lookup genv env name
eval genv env (If b t e) =
  let bv = eval genv env b
   in case bv of
    B True -> eval genv env t
    B False -> eval genv env e
    _ -> error ("not a bool: " ++ show bv)
eval genv env (PrimLam name argnames) =
  let args = map (lookup genv env) argnames
   in dispatchPrim name args

dispatchPrim :: Name -> [Value] -> Value
dispatchPrim "==" [a, b] = B (a == b)
dispatchPrim "+1" [I i] = I (i + 1)
dispatchPrim "-" [I a, I b] = I (a - b)
dispatchPrim "*" [I a, I b] = I (a * b)

-- r = eval genv M.empty (App (Var "fib") (Const (I 10)))
