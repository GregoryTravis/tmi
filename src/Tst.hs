module Tst
( tst ) where

import Util

tst :: Bool -> IO ()
tst b = do
  -- msp b
  massert "" b
