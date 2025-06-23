module Tuple
( tupleFromList
, checkSameTupleType
, tupleLength
, assertTupleLength
, applyPair
, mkPair ) where

import Data.List (foldl')
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Name
import Util
import Value

emptyTuple :: Value
emptyTuple = Tuple (TTuple []) []

tupleFromList :: [Value] -> Value
tupleFromList values = Tuple (TTuple typs) values
  where typs = map typOf values

mkPair :: Value -> Value -> Value
mkPair x y = tupleFromList [x, y]

checkSameTupleType :: Typ -> Typ -> a -> a
checkSameTupleType (TTuple xt) (TTuple yt) = assertM err ok
  where ok = xt == yt
        err = "type mismatch " ++ show xt ++ " " ++ show yt

tupleAt :: Value -> Int -> Value
tupleAt (Tuple _ values) i = values !! i

tupleLength :: Value -> Int
tupleLength (Tuple _ vals) = length vals

assertTupleLength :: Int -> Value -> a -> a
assertTupleLength len v = assert (tupleLength v == len)

applyPair :: (Value -> Value -> a) -> Value -> a
applyPair f tup = chk (f (tupleAt tup 0) (tupleAt tup 1))
  where chk = assertTupleLength 2 tup
