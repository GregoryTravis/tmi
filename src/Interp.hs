module Interp
( mkInterp ) where

import Util
import Val

mkInterp :: Env -> BuiltinDefs -> Interp
mkInterp = Interp
