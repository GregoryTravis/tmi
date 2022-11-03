module Tracer
( mkListTracer ) where

import Data.IORef
import System.IO.Unsafe

import Util

warningLength = 100 :: Int

-- To use a list tracer, shove in a value with Just each time, and finish with a
-- Nothing, at which point it dumps the list. Also warns if the list gets too long.

mkListTracer :: Show a => String -> IO (Maybe a -> IO ())
mkListTracer label = do
  as <- newIORef []
  return (accept label as)

accept :: Show a => String -> IORef [a] -> Maybe a -> IO ()
accept label asRef (Just a) = do
  modifyIORef asRef (++[a])
  warnMaybe asRef
accept label asRef Nothing = do
  as <- readIORef asRef
  msp $ "List tracer: " ++ label
  msp $ "---- start"
  mapM_ msp as
  msp $ "length: " ++ show (length as)
  msp $ "---- end"

warnMaybe :: IORef [a] -> IO ()
warnMaybe ref = do
  as <- readIORef ref
  if length as >= warningLength
    then msp "WARNING: long chain"
    else return ()
