module StateTmi
( addCalls
) where

import Lib
import Tmi

addCalls :: Show ww => V (W ww (TMI ww ())) -> V [Call] -> TMI (W ww (TMI ww ())) ()
addCalls vw clls =
  let db = _db <$$> vw
      -- rpc = (_rpc <$$> vw :: V (Rpc (TMI ww ())))
      rpc = _rpc <$$> vw
      calls = _calls <$$> rpc
   in calls <--- appendV <**> calls <$$> clls
