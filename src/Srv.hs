module Srv
( srv ) where

import Imp
import TMI
import Ty
import Util

srv :: (String -> TMI w String) -> TMI w ()
srv handle = do
  (path, tag) <- call $ getRequest 8000
  resp <- handle path
  call $ msp $ "path " ++ show path
  call $ msp $ "resp " ++ show resp
  call $ respondWith tag resp
  srv handle
