module Meta
( metaMain ) where

import System.Environment (getArgs)

import Core
import MainLoop
import Util

-- Above-top-level commands for driving the virtual machine
metaMain :: (Read w, Show w) => App w -> IO ()
metaMain app = do
  let go dbdir ["reset"] = reset dbdir
      go dbdir ["run"] = run app dbdir
      go dbdir ["injectRetval", indexS, val] = injectEvent dbdir app (Retval (read indexS) val)
      go dbdir ("injectCommand" : command) = injectEvent dbdir app (Command command)
  (dbdir : thing) <- getArgs
  ensureDbDir dbdir (initialW app)
  go dbdir thing
