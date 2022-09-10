{-# Language StandaloneDeriving, NamedFieldPuns #-}

module H where

import qualified CoatCheck as CC
import Lens
import TMI
import Ty
import Util
import VReadShow

initHistory :: V w (TMI w ()) -> w -> H w
initHistory main w = H { calls = CC.empty, events = [], generations = [w], todo = [cpsMain] }
  where cpsMain = vcps main

deriving instance Show w => Show (H w)

fcalls app = field app "calls" calls $ \r calls -> r { calls }
fevents app = field app "events" events $ \r events -> r { events }
fgenerations app = field app "generations" generations $ \r generations -> r { generations }
ftodo app = field app "todo" todo $ \r todo -> r { todo }
