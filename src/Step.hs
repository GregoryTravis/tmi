module Step
( applyContinuation ) where

import Data.Dynamic

import Util
import V
import Ty

applyContinuation :: Step w -> Retval -> TMI w ()
applyContinuation (Step _ k) (Retval s) = k (read s)
