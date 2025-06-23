module Rel
( emptyRel
, relFromList
, relToList
, relToGrid
, projRel
, joinRels ) where

import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S

import Name
import Rec
import Util
import Tuple
import Value

emptyRel :: Typ -> Value
emptyRel typ = Rel typ S.empty

relFromList :: [Value] -> Value
relFromList recs | null recs = error "relFromList must take a non-empty list"
                 | otherwise = foldl' addRecToRel (emptyRel typ) recs
                     where typ = trecToTrel $ typOf (head recs)

addRecToRel :: Value -> Value -> Value
addRecToRel rel@(Rel (TRel typMap) recs) rec = checkSameRecType typMap recType (Rel (TRel typMap)  recs')
  where recs' = S.insert rec recs
        recType = case typOf rec of TRec map -> map
addRecToRel x y = error ("um " ++ show x ++ " " ++ show y)

-- TODO could check the type and start off with an emptyRel
-- rel -> names -> rel
projRel :: Value -> [Name] -> Value
projRel rel keys = relMap rel (\rec -> projRec rec keys)

relMap :: Value -> (Value -> Value) -> Value
relMap (Rel _ recSet) f = relFromList recList'
  where recList = S.toList recSet
        recList' = map f recList

relToList :: Value -> [Value]
relToList (Rel _ recs) = S.toList recs

-- Render a relation as a header row and rows of values.
relToGrid :: Value -> [[Value]]
relToGrid r@(Rel _ recs) =
  let recsList = S.toList recs
      names = recNames (head recsList)
      values = map (recAts names) recsList
   in [map S names] ++ values

joinRels :: Value -> Value -> Value
joinRels a b =
  let commonFields = recLikeCommonFields a b
      tuples = [mkPair ar br | ar <- relToList a, br <- relToList b]
      filteredTuples = filter (applyPair (recMatchOn commonFields)) tuples
      merged = map (applyPair mergeRecs) filteredTuples
   in relFromList merged
