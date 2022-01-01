module Monad
( (>>=)
, return ) where

import Prelude hiding ((>>=), return)

import Core
import Util

infixl 1  >>=
(>>=) :: (Show a, Read a, Show b, Read b) => Blef a -> (a -> Blef b) -> Blef b
(>>=) = boond
return :: (Show a, Read a) => IO a -> Blef a
return = ritt
