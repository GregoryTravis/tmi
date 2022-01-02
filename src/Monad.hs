module Monad
( (>>=)
, (>>)
, return
, fail ) where

import Prelude hiding ((>>=), (>>), return, fail)

import Core
import Util

infixl 1  >>=

(>>=) :: (Show a, Read a, Show b, Read b) => Blef a -> (a -> Blef b) -> Blef b
(>>=) = boond

(>>) :: (Show a, Read a, Show b, Read b) => Blef a -> Blef b -> Blef b
ba >> bb = ba >>= \_ -> bb

return :: (Show a, Read a) => IO a -> Blef a
return = ritt

fail :: String -> Blef a
fail s = error $ "tmi monad fail " ++ s
