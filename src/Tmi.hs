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
, F(..)
, F2(..)
, F_1_2(..)
, hoist_1_1
, hoist_2_1
, hoist_1_2
, konstV
, undy -- Just for debugging in the absence of an evaluator
, Typeable
, Evaluator(..)
) where

import Data.Dynamic

import Internal
