module Flags
( Flags(..)
, flags
, veesp
, veeesp
, vmsp ) where

import Util

data Flags = Flags
  { verboseExecution :: Bool
  }

flags :: Flags
flags = Flags
  { verboseExecution = False
  }

-- Yeah these shouldn't be here
-- ifv :: Show s => (a -> b -> b) -> (a -> b -> b)
veesp :: Show s => s -> a -> a
veesp s a =
  if verboseExecution flags
    then eesp s a
    else a

veeesp :: (Show s, Show a) => s -> a -> a
veeesp s a =
  if verboseExecution flags
    then eeesp s a
    else a

vmsp :: Show a => a -> IO ()
vmsp a =
  if verboseExecution flags
    then msp a
    else return ()
