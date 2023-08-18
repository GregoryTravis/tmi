module GHCICleanup
( registerCleanup
, runCleanups ) where

import Data.IORef
import System.IO.Unsafe

import Util

cleanups :: IORef [IO ()]
cleanups = unsafePerformIO $ newIORef []

registerCleanup :: IO () -> IO ()
registerCleanup cleanup = modifyIORef cleanups (cleanup:)

runCleanups :: IO ()
runCleanups = do
  cs <- readIORef cleanups
  mapM_ id cs
  writeIORef cleanups []
