module Monad
( Core(..)
, Step(..) ) where

import Util
import V
import Ty

-- The core language consists of steps, each with its continuation.
data Core w = One (Step w) (Core w) | Done

data Step w = Assign (Write w) | Call (IO ())
