{-# LANGUAGE
  ExistentialQuantification
, MultiParamTypeClasses
, RankNTypes
#-}

module TIO where

{-
import Data.List (sortOn)

import Dyno
import Internal
import Tmi

-- Most recent is first
data Stream a = Stream [a]

append :: a -> Stream a -> Stream a
append a (Stream as) = Stream (a : as)

type TStream a = Stream (Int, a)

tappend :: Int -> a -> TStream a -> TStream a
tappend t a (Stream as) = Stream ((t, a) : as)

data Wrapped = Wrapped Dyno

wrapTStream :: (Eq a, Show a, Typeable a) => TStream a -> TStream Wrapped
wrapTStream (Stream tas) = Stream (map (onSnd (Wrapped . mkDyno)) tas)
  where onSnd f (a, b) = (a, f b)
-}

----data Wrapped = forall a. Wrapped a

--data Email = Email
--  { address :: String
--  , subject :: String
--  , body :: String } deriving (Eq, Show)

--wrapEmail :: Int -> Email -> MergeListE Int
--wrapEmail t e = MergeListE "email" t (mkDyno e)

---- String: email address
--data Invitation = Invitation String deriving (Eq, Show)

--wrapInvitation :: Int -> Invitation -> MergeListE Int
--wrapInvitation t inv = MergeListE "invitation" t (mkDyno inv)

--data MergeListE o = Ord o => MergeListE
--  { key :: String
--  , getOrd :: o
--  , wrapped :: Dyno }

--mergeLists :: Ord o => [MergeListE o] -> [MergeListE o] -> [MergeListE o]
--mergeLists ml0 ml1 = sortOn getOrd (ml0 ++ ml1)

--emails = [
--  Email 

------------------------

-- -- Two examples of rpcs: sending an email and inviting a user. The latter of
-- -- course can take arbitrarily long, but we want the same form for both.
-- data EmailRequest = EmailRequest
--   { address :: String
--   , subject :: String
--   , body :: String }
-- -- Bool: success
-- data EmailResponse = EmailResponse Bool
-- doEmail :: EmailRequest -> IO EmailResponse
-- doEmail = undefined

-- -- String: email address
-- data InvitationRequest = InvitationRequest String
-- -- TODO: add Timeout
-- data InvitationResponse = Accept | Reject
-- doInvitation :: InvitationRequest -> IO InvitationResponse
-- doInvitation = undefined

-- -- Wrap a request
-- data GRequest = forall req resp. GRequest
--   { greq :: req
--   , sender :: req -> IO resp }
-- wrapRequest :: req -> (req -> IO resp) -> GRequest
-- wrapRequest = GRequest

-- -- Timestamped append-only list
-- type Timestamp = Integer
-- -- export this closed
-- newtype TAOL a = TAOL [(Timestamp, a)]
-- aolAdd :: V (TAOL a) -> a -> TMI h w ()
-- aolAdd = undefined
-- -- aolAdd aol x = do
-- --   now <- getTimestamp
-- --   aol <-- aol ++ [(now, x)]

-------------------------

-- import Control.Monad.IO.Class (liftIO)

--import Tmi
--import Util

---- TODO Make this UTCTime from 'time'
--type Time = Int

---- Inviting a user
--data InvitationReq = InvitationReq String String
---- TODO timeout
--data InvitationResponse = Accept | Reject

----instance GDialog 

--data SenderReceiver h w = forall a. SenderReceiver (IO a) (a -> TMI h w ())
--runSenderReceiver :: SenderReceiver h w -> TMI h w ()
--runSenderReceiver (SenderReceiver action response) = do
--  a <- liftIO action
--  response a

--class GDialog d where
--  getSenderReceiver :: d -> SenderReceiver h w

---- A single request and pending or actual response
--data Dialog req resp = Dialog
--  { request :: req
--  , requestTime :: Time
--  , response :: Maybe resp
--  , responseTime :: Maybe Time }

---- A sequence of Dialogs. Always sorted by request time
--type DialogRecord req resp = [Dialog req resp]

---- The idea is to merge all DialogRecords into a single stream, but only the
---- completed ones, sorted by responseTime. This is a stream (append-only list),
---- so we can watch it for new entries and when they show up, we run the related
---- action.
---- TODO better name
----data UnifiedDialogs = Dialog d => 
