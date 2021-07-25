module W where

data W d = W
  { db :: d
  , rpc :: Rpc }
  deriving Show
_db = mkFielder "_db" db $ \w a -> w { db = a }
_rpc = mkFielder "_rpc" rpc $ \w a -> w { rpc = a }

