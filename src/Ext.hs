{-# LANGUAGE ExistentialQuantification, NumericUnderscores, RecordWildCards #-}

module Ext
( extMain
) where

-- import Data.Dynamic
import Data.Time.Clock (UTCTime, getCurrentTime)
-- import Data.Typeable
import Control.Monad.State hiding (lift, execState)

import Lib
import Rpc
import Tmi
import Util

-- data Request a = Request (IO a)

newtype Ext a = Ext (IO a)

data Initiation = Initiation ExecId deriving (Eq, Show)

data Req = Req Float String deriving Show
data Resp = Resp String deriving Show

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

