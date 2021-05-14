{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Tmi
( W(..)
, R(..)
, Write
, V(..)
, Receiver(..)
, hybrid1
, hybrid2
, (<--)
, (<**>)
, (<$$>)
, r
, wr
) where

import Data.Dynamic

import Curry
