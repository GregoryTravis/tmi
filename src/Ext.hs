{-# LANGUAGE ExistentialQuantification, NumericUnderscores, RecordWildCards #-}

module Ext
( Rpc(..)
, Req(..)
, Resp(..)
, Call(..)
, Initiation(..)
) where

-- import Data.Dynamic
import Data.Time.Clock (UTCTime, getCurrentTime)
-- import Data.Typeable
import Control.Monad.State hiding (lift, execState)

import ExecId
-- import Lib
-- import Rpc
-- import Tmi
import UniqueId
import Util

-- data Request a = Request (IO a)

newtype Ext a = Ext (IO a)

newtype Initiation = Initiation ExecId deriving (Eq, Show)

data Req = Req Float String deriving Show
newtype Resp = Resp String deriving (Eq, Show)

data Rpc = Rpc
  { calls :: [Call]
  , toExt :: Req -> IO Resp
  -- , toTmi :: Resp -> TMI WW ()
  }

instance Show Rpc where
  -- TODO do not love this
  show rpc = "RPC " ++ show (calls rpc)

data Call = Call
  { callUniqueId :: UniqueId
  , req :: Req
  , initiation :: Maybe Initiation
  , resp :: Maybe Resp
  , consquenceEnacted :: Bool
  } deriving Show

showRpc :: Rpc -> String
showRpc rpc = show $ map showCall (calls rpc)
  where showCall Call {..} = show (callUniqueId, req, resp)
