module Interp
( mkInterp ) where

import Util
import Val

mkInterp :: History -> Outside -> Interp
mkInterp = Interp
