module History
( add
, latest
, mkHistory ) where

import Util
import Val

mkHistory :: Val -> History
mkHistory initRoot = History [initRoot]

latest :: History -> Val
latest (History vs) = headF vs

add :: History -> Val -> History
add (History xs) x = History (x : xs)
