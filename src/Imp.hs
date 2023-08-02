{-# LANGUAGE RecordWildCards #-}

module Imp where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Data.IORef
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as M
import System.IO.Unsafe

import qualified CoatCheck as CC
import TMI
import Util
import Ty

type Request = String
data Response = Response

-- TODO
-- finish this
-- generic substitute/registry thing: given a recipe, create or fetch; also delete. Generic lib for it, all methods taking ioref, then a module has to just partially apply the funs to a single ioref
-- wait, we can't send an mvar, we need a substitute for that too

data Imp a b = Imp
  { requestStream :: Chan (a, CC.Tag)
  , responders :: IORef (CC.CoatCheck (Responder b))
  }

newImp :: ((a -> IO b) -> IO ()) -> IO (Imp a b)
newImp serverStarter = do
  aRequestStream <- newChan
  aResponders <- newIORef CC.empty
  let imp = Imp { requestStream = aRequestStream, responders = aResponders }
  forkIO (serverStarter (mkHandler imp))
  return imp

mkHandler :: Imp a b -> (a -> IO b)
mkHandler imp = do
  let handler req = do
        responder <- newResponder
        reses <- readIORef (responders imp)
        let (tag, newReses) = CC.check reses responder
        writeIORef (responders imp) newReses
        msp $ "Checked responder at " ++ show tag
        -- Server {..} <- getServer port
        let package = (req, tag)
        writeChan (requestStream imp) package
        res <- waitResponder responder
        return res
   in handler

data Responder r = Responder (MVar r)

newResponder :: IO (Responder r)
newResponder = do
  mvar <- newEmptyMVar
  return $ Responder mvar

waitResponder :: Responder r -> IO r
waitResponder (Responder mvar) = takeMVar mvar

-- servers :: IORef (M.Map Int (Server Request))
-- servers = unsafePerformIO $ newIORef M.empty

-- responders :: IORef (CC.CoatCheck (Responder res))
-- responders = unsafePerformIO $ newIORef CC.empty

-- Usage from TMI:
-- serve :: Int -> TMI ()
-- serve port = do
--   (request, responder) <- call $ getRequest port
--   response <- doStuff request
--   respondWith responder response

-- Usage from web server:
-- handler :: Server -> Request -> IO Response
-- handler (Server {..}) request = do
--   mvar = newEmptyMVar
--   writeChan requestStream (request, mvar)
--   takeMVar mvar
-- registerHandler "/" (handler theServer)

-- getServer :: Int -> IO (Server Request)
-- getServer port = do
--   svs <- readIORef servers
--   case M.lookup port svs of
--     Just server -> return server
--     Nothing -> do
--       requestStream <- newChan
--       let server = Server {..}
--           newServers = M.insert port server svs
--       writeIORef servers newServers
--       threadId <- forkIO $ startWebServer (handler' port) port
--       msp $ "Started server on " ++ show port ++ " thread " ++ show threadId
--       return server

-- handler' :: Int -> String -> IO String
-- handler' port req = do
--   responder <- newResponder
--   reses <- readIORef responders
--   let (tag, newReses) = CC.check reses responder
--   writeIORef responders newReses
--   msp $ "Checked responder at " ++ show tag
--   Server {..} <- getServer port
--   let package = (req, tag)
--   writeChan requestStream package
--   res <- waitResponder responder
--   return res

-- ensureServer :: Int -> IO ()
-- ensureServer port = do
--   getServer port
--   return ()

getRequest :: Imp a b -> IO (a, CC.Tag)
getRequest imp = readChan (requestStream imp)

respondWith :: Read a => Imp a b -> CC.Tag -> b -> IO ()
respondWith imp tag res = do
  cc <- readIORef (responders imp)
  msp $ "Retreiving responder from " ++ show tag
  let (Responder mvar, newCC) = fromJust $ CC.retrieve cc tag
  writeIORef (responders imp) newCC
  putMVar mvar res
