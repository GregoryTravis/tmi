{-# language OverloadedStrings #-}

module Web
( runServer ) where

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

runServer :: IO ()
runServer = run 8000 app

app :: App ()
app = do
  route "/" handler

handler :: Handler W.Response
handler = do
  p <- getPath
  liftIO $ msp p
  let s = "heyo"
  return $ toResponse (T.pack s, ok200, M.fromList [("Content-type", ["text/html"])] :: HeaderMap)
