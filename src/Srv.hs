module Srv
( srv ) where

import Imp
import TMI
import Ty
import Util

srv :: TMI w ()
srv = do
  (path, tag) <- call $ getRequest 8000
  let resp = handle path
  call $ msp $ "path " ++ show path
  call $ msp $ "resp " ++ show resp
  call $ respondWith tag resp
  srv

handle :: String -> String
handle s = s ++ " respy"
