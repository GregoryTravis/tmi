module Consequences
( consequencesMain ) where

import TmiPrelude
import Prelude (IO, ($))

import Util

--consequencesMain :: IO ()
consequencesMain = do

  let --d :: Delta Double
      d = Delta (DDoubleAdd 25)

  --    -- I wish this were: (snd $ somePairs !! 1) !! 2
  let --zoom :: E.Lens E.W Double
      zoom = (snd $ (somePairs !! 1)) !! 2

  msp $ dwrite zoom d
  msp $ read zoom

  msp "You've been consequenced."
