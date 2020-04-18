module TmiPrelude
( read
, dwrite
, somePairs
, (!!)
, snd
, E.DDoubleAdd(..)
, E.Delta(..)
, Double
-- , w
) where

import Prelude ()
import qualified Prelude as P

import qualified Existential as E
import Util

somePairs = E.somep

w :: E.W
w = E.W { E.anInt = 10
        , E.aDouble = 3.3
        , E.aList = [1, 2, 3]
        , E.somePairs = [(100, [1, 2, 3]), (200, [2, 3, 4])] }
read = E.read' w
dwrite = E.dwrite' w

(!!) = (E.!!-)

snd = E.sndF

type Double = E.Lens E.W P.Double
