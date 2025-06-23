module Main where

import Data.Dynamic
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as M

import Code
import Display
import Fd
import Name
import Old
import OptLam
import Rec
import Rel
import Tuple
import Util
import Value

tests = do
  fdTests
  putStr "tests ok\n"

stuffMain = do
  tests
  let rel0 = relFromList [recFromList [("foo", I 3), ("bar", S "three"), ("baz", I 30)],
                          recFromList [("foo", I 4), ("bar", S "four"), ("baz", I 40)],
                          recFromList [("foo", I 5), ("bar", S "five"), ("baz", I 50)]]
  let rel1 = relFromList [recFromList [("bar", S "three"), ("baz", I 30)],
                          recFromList [("bar", S "three"), ("baz", I 300)],
                          recFromList [("bar", S "four"), ("baz", I 40)],
                          recFromList [("bar", S "four"), ("baz", I 400)],
                          recFromList [("bar", S "five"), ("baz", I 50)]]
     
  putStr (displayValue rel0)
  putStr (displayValue rel1)
  putStr (displayValue (projRel rel0 ["foo", "bar"]))
  putStr (displayValue (projRel rel0 ["bar", "baz"]))
  putStr (displayValue (projRel rel0 ["bar"]))
  let t0 = tupleFromList [S "a", I 12, S "b"]
  putStr (displayValue t0)
  putStr (displayValue (joinRels rel0 rel1))
  putStr (displayValue (Adt (TAdt "FooBar" [TCtor "Foo" [TI], TCtor "Bar" [TI, TS]]) "Bar" [I 12, S "thirtenn"]))

main = do
  --stuffMain
  oldMain
  --optLamMain
