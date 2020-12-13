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

-- Ideal interface for dialogs
data Dialog req resp
mkDialog :: (req -> IO resp) -> V (Dialog req resp)
mkDialog = undefined
getAll :: Dialog req resp -> Stream (req, Maybe resp)
getAll = undefined
getCompleted :: Dialog req resp -> Stream (req, resp)
getCompleted = undefined

-- data Dialog req resp = Dialog (req -> IO resp) [req] [resp]
-- addDialog :: Dialog req resp -> TMI h w ()
-- addDialog dialog = do
--   return ()

-- -- TODO I guess we don't care about the tag, so maybe it should be generated
-- -- Maybe pass in the input queue here? Otherwise where do we put it?
-- addDialog :: String -> (req -> IO resp) -> V req -> TMI h w (V (Stream resp))
-- addDialog action = do
--   dialogManager <- getDialogManager
--   putDialogManager dialogManager

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

---- A queue of heterogeneously-typed rpcs
----
---- t: Eq, used to distinguish the typed queues. All entries with a single tag
----    are the same type (though not vice versa)
--data DynQueue t = DynQueue

--data Dialog req resp = Dialog [req] [resp]

---- TODO also should be a method of the TMI monad, returning two (V (Stream a))
--addQueue :: (req -> IO resp) -> DynQueue t -> DynQueue t
--addQueue = undefined
