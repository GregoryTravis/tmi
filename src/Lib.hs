module Lib where

import Lift
import Ty
import Util
import V

(!!.) :: V w [a] -> V w Int -> V w a
(!!.) = lift2 $ nuni "!!." (!!)

-- untested
-- vcons :: V w a -> V w [a] -> V w [a]
-- vcons = lift2 $ nuni "vcons" (:)
