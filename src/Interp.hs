module Interp
( mkInterp ) where

import Lambda
import Util

mkInterp :: Env -> BuiltinDefs -> Interp
mkInterp = Interp
