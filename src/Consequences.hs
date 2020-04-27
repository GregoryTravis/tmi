module Consequences
( consequencesMain ) where

import TmiPrelude
import Prelude (IO, ($))
import qualified Prelude as P

import Util

consequencesMain :: Umm ()
consequencesMain = do
  encoder 2 (strings !! 1) <-+ (Prepend "sss")
  ints !! 1 <-- 35
