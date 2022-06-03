{-# Language NamedFieldPuns #-}

module W
( fwApp
, fwSys ) where

import Lens
-- import Sys
import Ty
import Util

fwApp :: V (W app) (W app) -> V (W app) app
fwApp w = field w "wApp" wApp $ \w wApp -> w { wApp }

fwSys :: V (W app) (W app) -> V (W app) (Sys (W app))
fwSys w = field w "wSys" wSys $ \w wSys -> w { wSys }
