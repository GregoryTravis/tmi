{-# language OverloadedStrings #-}

-- TODO rename this to Web and figure out why it has a warning when you do.
module Veb
( startWebServer ) where

import Control.Concurrent
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
-- import Network.HTTP.Types.Status
import Network.Wai as W
import Web.Firefly

import Util

startWebServer :: Int -> (String -> IO String) -> IO ()
startWebServer port handler' = do
  let app :: App ()
      app = do
        route ".*" (handler handler')
  run port app

handler :: (String -> IO String) -> Handler W.Response
handler handler' = do
  tp <- getPath
  let p = T.unpack tp
  liftIO $ msp p
  s <- liftIO $ handler' p
  return $ toResponse (T.pack s, ok200, M.fromList [("Content-type", ["text/html"])] :: HeaderMap)
