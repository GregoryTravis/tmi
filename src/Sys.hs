{-# Language NamedFieldPuns #-}

module Sys
( fsysLog  ) where

import Lens
import Log
import Ty
import Util

fsysLog :: V w (Sys w) -> V w (Log w)
fsysLog sys = field sys "sysLog" sysLog $ \w sysLog -> w { sysLog }
