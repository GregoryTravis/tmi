module Adt
( adtTyToBuiltinDefs ) where

import Builtin
import Util
import Val

-- Convert an ADT type to BuiltinDefs for its constructors
adtTyToBuiltinDefs :: Ty -> [BuiltinDef]
adtTyToBuiltinDefs (TAdt _ ctors) = map adtTyCtorToBuiltinDef ctors

-- Convert an ADT ctor to a BuiltinDefs
adtTyCtorToBuiltinDef :: TyCtor -> BuiltinDef
adtTyCtorToBuiltinDef (TyCtor name []) = BuiltinDef name 0 (lyft0 (mkCtor0 name) id)
adtTyCtorToBuiltinDef (TyCtor name [arg0]) = BuiltinDef name 1 (lyft1 (mkCtor1 name) id id)
adtTyCtorToBuiltinDef (TyCtor name [arg0, arg1]) = BuiltinDef name 2 (lyft2 (mkCtor2 name) id id id)

mkCtor0 :: String -> Val
mkCtor0 name = dkv $ Cton name []
mkCtor1 :: String -> Val -> Val
mkCtor1 name a = dkv $ Cton name [a]
mkCtor2 :: String -> Val -> Val -> Val
mkCtor2 name a b = dkv $ Cton name [a, b]
