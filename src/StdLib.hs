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

aabOp :: String -> (Val -> Val -> Bool) -> BuiltinDef
aabOp name f = BuiltinDef name 2 (lyft2 f id id kB)

adts :: [Ty]
adts =
  [ TAdt "List" [TyCtor "Cons" [DK, DK], TyCtor "Nil" []]
  , TAdt "Yeah" [TyCtor "Boo" [DK], TyCtorRec "Loo" [("foo", DK), ("bar", DK)]]
  ]

ctorEnv :: Env
ctorEnv = MapLayer $ M.fromList $
  let tyCtors = concat $ map getCtors adts
      names = map getName tyCtors
      ctors = map (Val DK . Code . mkCtor) tyCtors
   in zip names ctors
      -- nameAndArities = map nameAndArity tyCtors
  where getCtors (TAdt _ ctors) = ctors
        getName (TyCtor name _) = name
        getName (TyCtorRec name _) = name

recDestructorEnv :: Env
recDestructorEnv = MapLayer $ M.fromList $
  let tyCtorRecs = concat $ map getTyCtorRecs adts
      fieldDestructors = concat $ map mkFieldDestructors tyCtorRecs
   in fieldDestructors
  where getTyCtorRecs (TAdt _ ctors) = filter isTyCtorRec ctors
        isTyCtorRec (TyCtor _ _) = False
        isTyCtorRec (TyCtorRec _ _) = True

mkFieldDestructors :: TyCtor -> [(Ident, Val)]
mkFieldDestructors (TyCtorRec ctorName fields) = map (mk ctorName) fields
  where mk ctorName (fieldName, _) = (fieldName, Val DK $ Code $ mkCtonIndexGetter ctorName fieldName fieldNames)
        fieldNames = map fst fields

mkCtonIndexGetter :: Ident -> Ident -> [Ident] -> Code
mkCtonIndexGetter ctorName field fields =
  let pat = map dkOrPatVar fields
      patVar = "f" ++ field
      dkOrPatVar f | f == field = (f, Val DK $ PatVar patVar)
                   | otherwise = (f, Val DK $ Underscore)
      ctonPat = Val DK $ CtonRec ctorName pat
      body = Id patVar
      clauses = [(ctonPat, body)]
   in Lam "x" (Case (Id "x") clauses)

theBuiltinDefs :: [BuiltinDef]
theBuiltinDefs =
  [ iiiOp "+" (+)
  , iiiOp "-" (-)
  , iiiOp "*" (*)
  , iibOp "<" (<)
  , iibOp ">" (>)
  , aabOp "==" (==)
  ]

nonBuiltins = MapLayer $ M.fromList $
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
  let builtinDefMap = BuiltinDefs $ M.fromList (map f theBuiltinDefs)
        where f bd@(BuiltinDef name _ _) = (name, bd)
      builtinEnv = MapLayer $ M.fromList (map f theBuiltinDefs)
        where f bd@(BuiltinDef name _ _) = (name, dkv $ Code $ wrapBuiltin bd)
      globalEnv = combineManyNoClash [nonBuiltins, builtinEnv, ctorEnv, recDestructorEnv]
      values = CtonRec "Values" (M.toList (case globalEnv of MapLayer x -> x))
      code = CtonRec "Code" [("values", Val DK values)]
      root = CtonRec "Root" [("code", Val DK code)]
   in mkInterp (History [Val DK root]) Outside { builtinDefs = builtinDefMap }
