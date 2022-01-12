module Monad
( (>>=)
, (>>)
, return
, fail ) where

import Prelude hiding ((>>=), (>>), return, fail)

import Core
import Util

infixl 1  >>=

(>>=) :: (Show a, Read a, Show b, Read b) => Blef w a -> (a -> Blef w b) -> Blef w b
(>>=) = boond

(>>) :: (Show a, Read a, Show b, Read b) => Blef w a -> Blef w b -> Blef w b
ba >> bb = ba >>= \_ -> bb

return :: (Show a, Read a) => IO a -> Blef w a
return = ritt

fail :: String -> Blef w a
fail s = error $ "tmi monad fail " ++ s
