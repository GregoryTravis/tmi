module StdLib (stdLib) where

import qualified Data.Map.Strict as M

import Adt
import Awkward
import Builtin
import Env
import Interp
import Util
import Val

iiiOp :: String -> (Int -> Int -> Int) -> BuiltinDef
iiiOp name f = BuiltinDef name 2 (lyft2 f unVI unVI kI)

iibOp :: String -> (Int -> Int -> Bool) -> BuiltinDef
iibOp name f = BuiltinDef name 2 (lyft2 f unVI unVI kB)

adts :: [Ty]
adts =
  [ TAdt "List" [TyCtor "Cons" [DK, DK], TyCtor "Nil" []]
  ]

ctorEnv :: Env
ctorEnv = Env $ M.fromList $
  let tyCtors = concat $ map getCtors adts
      nameAndArities = map nameAndArity tyCtors
   in map (\(name, arity) -> (name, Val DK $ Code $ mkCtor name arity)) nameAndArities
  where getCtors (TAdt _ ctors) = ctors
        nameAndArity (TyCtor name args) = (name, length args)

builtinDefs :: [BuiltinDef]
builtinDefs =
  [ (BuiltinDef "+" 2 (lyft2 (+) unVI unVI kI))
  , (BuiltinDef "-" 2 (lyft2 (-) unVI unVI kI))
  , (BuiltinDef "*" 2 (lyft2 (*) unVI unVI kI))
  , iiiOp "+" (+)
  , iiiOp "-" (-)
  , iiiOp "*" (*)
  , iibOp "<" (<)
  , iibOp ">" (>)
  , (BuiltinDef "==" 2 (lyft2 (==) id id kB))
  ]

nonBuiltins = Env $ M.fromList $
  [ ("add1", Val (TFun TI (TFun TI TI))
                 $ Code $ Lam "x" (app2 (Id "+") (Id "x") (ckI 1)))
  , ("sub1", Val (TFun TI (TFun TI TI))
                 $ Code $ Lam "x" (app2 (Id "-") (Id "x") (ckI 1)))
  , ("fact", Val (TFun TI TI)
                 $ Code $ Lam "x" (If (app2 (Id "==") (Id "x") (ckI 0))
                                      (ckI 1)
                                      (app2 (Id "*") (Id "x")
                                            (app1 (Id "fact")
                                                  (app2 (Id "-") (Id "x")
                                                        (ckI 1))))))
  , ("head", Val DK $ Code $
      Lam "x" (Case (Id "x") [(Val DK (Cton "Cons" [Val DK (PatVar "x"), Val DK (PatVar "xs")]), Id "x"),
                              (Val DK (Cton "Nil" []), CVal (kS "error: head of empty list"))]))
  , ("tail", Val DK $ Code $
      Lam "x" (Case (Id "x") [(Val DK (Cton "Cons" [Val DK (PatVar "x"), Val DK (PatVar "xs")]), Id "xs"),
                              (Val DK (Cton "Nil" []), CVal (kS "error: tail of empty list"))]))
  , ("map", Val DK $ Code $
      Lam "f" (Lam "xs" (Case (Id "xs")
        [ (Val DK (Cton "Cons" [Val DK (PatVar "x"), Val DK (PatVar "xs")]),
           app2 (Id "Cons") (App (Id "f") (Id "x")) (app2 (Id "map") (Id "f") (Id "xs")))
        , (Val DK (Cton "Nil" []), CVal (Val DK (Cton "Nil" [])))])))
  , ("filter", Val DK $ Code $
      Lam "f" (Lam "xs" (Case (Id "xs")
        [ (Val DK (Cton "Cons" [Val DK (PatVar "x"), Val DK (PatVar "xs")]),
           (If (App (Id "f") (Id "x"))
               (app2 (Id "Cons") (Id "x") (app2 (Id "filter") (Id "f") (Id "xs")))
               (app2 (Id "filter") (Id "f") (Id "xs"))))
        , (Val DK (Cton "Nil" []), CVal (Val DK (Cton "Nil" [])))])))
  ]

stdLib :: Interp
stdLib =
  let builtinDefMap = BuiltinDefs $ M.fromList (map f builtinDefs)
        where f bd@(BuiltinDef name _ _) = (name, bd)
      builtinEnv = Env $ M.fromList (map f builtinDefs)
        where f bd@(BuiltinDef name _ _) = (name, dkv $ Code $ wrapBuiltin bd)
      globalEnv = combineManyNoClash [nonBuiltins, builtinEnv, ctorEnv]
   in mkInterp globalEnv builtinDefMap
