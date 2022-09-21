{-# Language NamedFieldPuns, RecordWildCards, StandaloneDeriving #-}

module H where

import qualified CoatCheck as CC
import Lens
import qualified NiceMap as NM
import Recon
import TMI
import Ty
import Util
import V
import VReadShow

initHistory :: V w (TMI w ()) -> w -> H w
initHistory main w = H { calls = CC.empty, events = [], generations = [w], todo = [cpsMain], store = NM.empty }
  where cpsMain = vcps main

deriving instance Show w => Show (H w)
deriving instance (HasRecon w, Read w) => Read (H w)

fcalls app = field app "calls" calls $ \r calls -> r { calls }
fevents app = field app "events" events $ \r events -> r { events }
fgenerations app = field app "generations" generations $ \r generations -> r { generations }
ftodo app = field app "todo" todo $ \r todo -> r { todo }

stats :: H w -> String
stats (H {..}) = unlines lines
  where lines =
          [ "calls " ++ show (CC.size calls)
          , "events " ++ show (length events)
          , "generations " ++ show (length generations)
          , "todo " ++ show (length todo)
          , "store " ++ show (NM.size store)
          , "todo lengths " ++ show todoLengths
          ]
        todoLengths = map vSize todo
