{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Tmi
( V
, Nice
, mkRoot
, F(..)
, F2(..)
, F_1_2(..)
, N(..) -- Just for debugging in the absence of an evaluator
, vN -- Just for debugging in the absence of an evaluator
, hoist_1_1
, hoist_2_1
, hoist_1_2
, konstV
, dy
, undy -- Just for debugging in the absence of an evaluator
, dyv
, Typeable
, Evaluator(..)
, History
, Listener
, Write(..)
) where

import Data.Dynamic

import Internal
