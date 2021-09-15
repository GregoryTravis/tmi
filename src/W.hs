module W where

import Ext
import Lens

data W d c = W
  { db :: d
  , rpc :: Rpc c }
  deriving Show
_db = mkFielder "_db" db $ \w a -> w { db = a }
_rpc = mkFielder "_rpc" rpc $ \w a -> w { rpc = a }
