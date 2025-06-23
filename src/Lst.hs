module Lst
( lstFromList ) where

import Display
import Rec
import Rel
import Tuple
import Util
import Value

lstFromList :: [Value] -> Value
lstFromList values | null values = error "lstFromList must take a non-empty list"
                   | otherwise = checkSameTypes (map typOf values) $ Lst (TLst (typOf (head values))) values
