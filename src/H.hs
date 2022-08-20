{-# Language NamedFieldPuns #-}
module H where

import Lens
import TMI
import Ty
import Util

initHistory :: V w (TMI w ()) -> w -> H w
initHistory main w = H { calls = [], events = [], generations = [w], todo = [cpsMain] }
  where cpsMain = vcps main

fcalls app = field app "calls" calls $ \r calls -> r { calls }
fevents app = field app "events" events $ \r events -> r { events }
fgenerations app = field app "generations" generations $ \r generations -> r { generations }
ftodo app = field app "todo" todo $ \r todo -> r { todo }
