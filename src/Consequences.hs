module Consequences
( consequencesMain ) where

import TmiPrelude
import Prelude (IO, ($))
import qualified Prelude as P

import Util

--consequencesMain :: IO ()
consequencesMain = do

  let d :: Delta P.Double
      d = Delta (DDoubleAdd 25)

  let zoom :: Double
      zoom = (snd $ (somePairs !! 1)) !! 2

  msp $ dwrite zoom d
  msp $ read zoom

  msp "You've been consequenced."
