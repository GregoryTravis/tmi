module ExecId
( ExecId
, currentExecId ) where

-- import System.Process (Pid)
import System.Posix.Process (getProcessID)
import System.Posix.Types (ProcessID)

-- TODO should have a timestamp too
newtype ExecId = ExecId ProcessID deriving (Eq, Show)

currentExecId :: IO ExecId
currentExecId = do
  pid <- getProcessID
  return $ ExecId pid
