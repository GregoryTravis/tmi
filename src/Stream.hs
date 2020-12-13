{-# LANGUAGE
  ExistentialQuantification
, MultiParamTypeClasses
, NumericUnderscores
, RankNTypes
, TupleSections
#-}

module Stream
( streamMain ) where

import Control.Concurrent
import Data.Dynamic
import Data.List (sortOn)
import Data.Maybe (fromJust)
import Data.Typeable

--import Internal
import Tmi
import Util

data EmailReq = EmailReq
  { address :: String
  , subject :: String
  , body :: String } deriving Show

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
data Invitation = Invitation String deriving Show

invitations = Stream
  [ (4, Invitation "a@b.com")
  , (2, Invitation "b@c.com") ]

----------------------------

-- Most recent is first
data Stream a = Stream [(Int, a)] deriving Show

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

type DynQueue = Stream (String, Dynamic)

addQueue :: Typeable a => String -> Stream a -> DynQueue -> DynQueue
addQueue tag stream dq = mergeStreams dq (dynStream stream)
  where dynStream = fmap ((tag,) . toDyn)

getStream :: Typeable a => DynQueue -> String -> Stream a
getStream dq tag = fmap (fromJust . fromDynamic . snd) (filterStream ((==tag) . fst) dq)

----------------------------

streamMain = do
  let dq = addQueue "invitations" invitations (addQueue "emails" emailReqs emptyStream)
      emailReqs' :: Stream EmailReq
      emailReqs' = getStream dq "emails"
      invitations' :: Stream Invitation
      invitations' = getStream dq "invitations"
  msp dq
  msp emailReqs'
  msp invitations'
  msp "stream hi"
