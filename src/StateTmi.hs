module StateTmi
( addCalls
) where

import Lib
import Tmi

addCalls :: Show ww => V (W ww) -> V [Call] -> TMI (W ww) ()
addCalls vw clls =
  let db = _db <$$> vw
      rpc = (_rpc <$$> vw :: V Rpc)
      calls = _calls <$$> rpc
   in calls <--- appendV <**> calls <$$> clls
