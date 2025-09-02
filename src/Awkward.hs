module Awkward where

import Builtin
import Env
import Interp
import Util
import Val

app1 :: Code -> Code -> Code
app1 = App

app2 :: Code -> Code -> Code -> Code
app2 f a b = CVal $ Val DK (Code (App (App f a) b))

mkList :: [Val] -> Val
mkList [] = Val DK (Cton "Nil" [])
mkList (x : xs) = Val DK (Cton "Cons" [x, mkList xs])

mkListCode :: [Code] -> Code
mkListCode [] = Id "Nil"
mkListCode (x : xs) = app2 (Id "Cons") x (mkListCode xs)
