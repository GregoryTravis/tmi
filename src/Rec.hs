module Rec
( recFromList
, recNames
, checkSameRecType
, projRec
, recAt
, recAts
, recLikeCommonFields
, mergeRecs
, recMatchOn ) where

import Data.List (foldl')
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Name
import Util
import Value

emptyRec :: Value
emptyRec = Rec (TRec M.empty) M.empty

recFromList :: [(Name, Value)] -> Value
recFromList bindings = foldr add emptyRec bindings

recNames :: Value -> [Name]
recNames (Rec _ vals) = M.keys vals

add :: (Name, Value) -> Value -> Value
add (n, v) rec@(Rec (TRec typMap) map) = check $ Rec (TRec typMap') map'
  where check x | not (n `M.member` map) = x
                | otherwise = error $ "Already contains " ++ show rec ++ " " ++ n
        map' = M.insert n v map
        typMap' = M.insert n t typMap
        t = typOf v

checkSameRecType :: M.Map Name Typ -> M.Map Name Typ -> a -> a
checkSameRecType xt yt = assertM err ok
  where ok = xt == yt
        err = "type mismatch " ++ show xt ++ " " ++ show yt

recAt :: Name -> Value -> Value
recAt name (Rec _ values) = values M.! name

recAts :: [Name] -> Value -> [Value]
recAts names (Rec _ values) = [values M.! name | name <- names]

recAtMaybe :: Name -> Value -> Maybe Value
recAtMaybe name (Rec _ values) = M.lookup name values

-- rec -> names -> rec
projRec :: Value -> [Name] -> Value
projRec (Rec (TRec typMap) map) newKeys = Rec (TRec (justKeys typMap newKeys)) (justKeys map newKeys)
  -- TODO make this signature allowed
  where -- justKeys :: M.Map k v -> [k] -> M.Map k v
        justKeys map keys = foldl' (\m n-> M.insert n (map M.! n) m) M.empty newKeys

-- Returns the type `Map` for either a rec or a rel
recLikeType (Rec (TRec typMap) _) = typMap
recLikeType (Rel (TRel typMap) _) = typMap

recLikeValues (Rec _ values) = values

-- Returns the set of common fields of two reclike types. If any field exists in both maps
-- with unequal types, an assertion failure is raised.
recTypeCommonFields :: (M.Map Name Typ) -> (M.Map Name Typ) -> [Name]
recTypeCommonFields tm0 tm1 = chk commonNames
  where commonNames = S.toList (S.intersection (S.fromList (M.keys tm0)) (S.fromList (M.keys tm1)))
        chk = assert sameTypes
        sameTypes = types0 == types1
        types0 = map (tm0 M.!) commonNames
        types1 = map (tm1 M.!) commonNames

recLikeCommonFields :: Value -> Value -> [Name]
recLikeCommonFields r0 r1 = recTypeCommonFields rt0 rt1
  where rt0 = recLikeType r0
        rt1 = recLikeType r1

recMatchOn :: [Name] -> Value -> Value -> Bool
recMatchOn names rec0 rec1 = values0 == values1
  where values0 = [recLikeValues rec0 M.! name | name <- names]
        values1 = [recLikeValues rec1 M.! name | name <- names]

-- Combine the keys + values of two Recs. For fields with the same name, the
-- types and values must match.
mergeRecs :: Value -> Value -> Value
mergeRecs rec0 rec1 =
  let allNames = S.toList (S.union (S.fromList (recNames rec0)) (S.fromList (recNames rec1)))
      values0 = [recAtMaybe name rec0 | name <- allNames]
      values1 = [recAtMaybe name rec1 | name <- allNames]
      values = zip values0 values1
      combined = map combine values
      rec' = recFromList (zip allNames combined)
      combine (Just x0, Just x1) | x0 == x1 = x0
                                 | otherwise = error ("mergeRecs: unequal common values: " ++ show x0 ++ " != " ++ show x1 ++ " in " ++ show rec0 ++ " and " ++ show rec1)
      combine (Just x0, _) = x0
      combine (_, Just x1) = x1
   in rec'
