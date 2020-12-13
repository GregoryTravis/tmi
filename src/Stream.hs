{-# LANGUAGE
  ExistentialQuantification
, MultiParamTypeClasses
, NumericUnderscores
, NamedFieldPuns
, RankNTypes
, RecordWildCards
, TupleSections
#-}

module Stream
( streamMain ) where

import Control.Concurrent
import Data.Dynamic
import Data.List (sortOn)
import Data.Maybe (fromJust)
import Data.Typeable

import History
import Tmi
import Util

data EmailReq = EmailReq
  { address :: String
  , subject :: String
  , body :: String } deriving (Show, Eq, Typeable)
--instance Nice EmailReq

type EmailResp = Bool

-- A typical rpc
sendEmail :: EmailReq -> IO EmailResp
sendEmail _ = do
  msp "Sending..."
  threadDelay $ 5 * 1_000_000
  return True

emailReqs :: Stream EmailReq
emailReqs = Stream
  [ (3, EmailReq "a@b.com" "hey" "sup?")
  , (1, EmailReq "b@c.com" "hi" "yo?") ]

-- String: email address
data Invitation = Invitation String deriving (Show, Eq, Typeable)

invitations = Stream
  [ (4, Invitation "a@b.com")
  , (2, Invitation "b@c.com") ]

----------------------------

-- Most recent is first
data Stream a = Stream [(Int, a)] deriving (Eq, Show)

emptyStream :: Stream a
emptyStream = Stream []

mergeStreams :: Stream a -> Stream a -> Stream a
mergeStreams (Stream xs) (Stream ys) = Stream (mergeEm xs ys)
  where mergeEm [] ys = ys
        mergeEm xs [] = xs
        mergeEm xs'@((tx, x) : xs) ys'@((ty, y) : ys)
          -- (>) because streams are stored in reverse order
          | tx > ty   = (tx, x) : mergeEm xs ys'
          | otherwise = (ty, y) : mergeEm xs' ys

filterStream :: (a -> Bool) -> Stream a -> Stream a
filterStream p (Stream tas) = Stream $ filter (p . snd) tas

instance Functor Stream where
  fmap f (Stream tas) =
    let (ts, as) = unzip tas
        ds = map f as
     in Stream (zip ts ds)

-- TODO get the time from the TMI monad
append :: Int -> a -> Stream a -> Stream a
append t a (Stream as) = Stream ((t, a) : as)

----------------------------

data IONexus = IONexus [DynStream] [DynStream]
initIONexus :: IONexus
initIONexus = IONexus [] []

type DynStream = Stream (String, Dynamic)

toDynStream :: Nice a => String -> Stream a -> DynStream
toDynStream tag = fmap ((tag,) . toDyn)

addQueue :: Nice a => String -> Stream a -> DynStream -> DynStream
addQueue tag stream dq = mergeStreams dq (toDynStream tag stream)

fromDynStream :: Typeable a => DynStream -> String -> Stream a
fromDynStream dq tag = fmap (fromJust . fromDynamic . snd) (filterStream ((==tag) . fst) dq)

-- _toDynStream :: Nice a => F2 String (Stream a) DynStream
-- _toDynStream = F2 "_toDynStream" toDynStream undefined
-- _toDynStream :: Nice a => V String -> V (Stream a) -> V DynStream
-- _toDynStream = hoist_2_1 $ F2 "_toDynStream" toDynStream undefined

----------------------------

data Rpc req resp = Rpc (req -> IO resp) (Stream req)
_rpc_1 :: F (Rpc req resp) (Stream req)
_rpc_1 = F "_rpc_1" (\(Rpc _ s) -> s) undefined

data W = W
  { ioNexus :: IONexus
  , rpc :: Rpc EmailReq EmailResp}
w :: W
w = W { ioNexus = initIONexus
      , rpc = Rpc sendEmail emptyStream }

_ioNexus :: F W IONexus
_ioNexus = F {..}
  where name = "_ioNexus"
        ffor W {..} = ioNexus
        frev w ioNexus = w { ioNexus }

addRpc :: V (Rpc req resp) -> V IONexus -> TMI h w ()
addRpc = undefined

streamMain = do
  let dq = addQueue "invitations" invitations (addQueue "emails" emailReqs emptyStream)
      emailReqs' :: Stream EmailReq
      emailReqs' = fromDynStream dq "emails"
      invitations' :: Stream Invitation
      invitations' = fromDynStream dq "invitations"
  msp dq
  msp emailReqs'
  msp invitations'
  msp "stream hi"
