module Srv
( srv ) where

import Data.IORef
import System.IO.Unsafe

import qualified CoatCheck as CC
import Imp
import TMI
import Ty
import Util
import Veb

srv :: (String -> TMI w String) -> TMI w ()
srv handler = do
  let go = do
            (path, tag) <- call getReq
            resp <- handler path
            call $ msp $ "path " ++ show path
            call $ msp $ "resp " ++ show resp
            call $ respWith tag resp
            go
  go

theImp :: IORef (Maybe (Imp String String))
theImp = unsafePerformIO $ newIORef Nothing

getOrMakeImp :: IO (Imp String String)
getOrMakeImp = do
  mimp <- readIORef theImp
  case mimp of
    Nothing -> do
      imp <- newImp (startWebServer 3000)
      writeIORef theImp (Just imp)
      return imp
    Just imp ->
      return imp

getReq :: IO (String, CC.Tag)
getReq = do
  imp <- getOrMakeImp
  getRequest imp

respWith :: CC.Tag -> String -> IO ()
respWith tag resp = do
  imp <- getOrMakeImp
  respondWith imp tag resp

-- loopImp :: (Read a, Show a, Show b) => Imp a b -> (a -> TMI w b) -> TMI w ()
-- loopImp imp handler = do
--   let go = do
--         (path, tag) <- call $ getRequest imp
--         resp <- handler path
--         call $ msp $ "path " ++ show path
--         call $ msp $ "resp " ++ show resp
--         call $ respondWith imp tag resp
--         go
--   go

