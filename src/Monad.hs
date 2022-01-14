module Monad
( (>>=)
, (>>)
, return
, fail ) where

import Prelude hiding ((>>=), (>>), return, fail)

import Core
import Util

infixl 1  >>=

(>>=) :: Blef w a -> (a -> Blef w b) -> Blef w b
(>>=) = boond

(>>) :: Blef w a -> Blef w b -> Blef w b
ba >> bb = ba >>= \_ -> bb

return :: a -> Blef w a
return = BReturn

fail :: String -> Blef w a
fail s = error $ "tmi monad fail " ++ s
