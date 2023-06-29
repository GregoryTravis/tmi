{-# LANGUAGE RecordWildCards #-}

module Imp where

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Data.IORef
import qualified Data.Map.Strict as M
import System.IO.Unsafe

import CoatCheck
import Util

data Request = Request
data Response = Response

-- TODO
-- finish this
-- generic substitute/registry thing: given a recipe, create or fetch; also delete. Generic lib for it, all methods taking ioref, then a module has to just partially apply the funs to a single ioref
-- wait, we can't send an mvar, we need a substitute for that too

data Responder r = Responder (MVar r)

data Server = Server req res
  { requestStream :: Chan (req, Responder res)
  }

servers :: IORef (Map Int (Server Request Response))
servers = unsafePerformIO newIORef

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

createServer :: Int -> IO Server
createServer port = do
  svs <- readIORef servers
  case M.lookup port svs of
    Just server -> return server
    Nothing -> do
      requestStream <- newChan
      let server = Server {..}
      newSvs = M.insert port server svs
      writeIORef servers newSvs
      return server

ensureServer :: Int -> IO ()
ensureServer port = do
  createServer port
  return ()

getRequest :: Int -> IO (Request, Responder)
getRequest port = do
  ensureServer port
  servers <- readIORef servers
  Server {..} = servers M.! port
  readChan requestStream

respondWith :: Responder r -> r -> IO ()
respondWith Responder mvar) response = putMVar mvar response
