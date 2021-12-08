module Monad
( ) where

import Util
import V
import Ty

-- ((), es') <- runStateT (runTMI action) es

{-
(<--) :: V w a -> V w a -> TMI w ()
lvalue <-- rvalue = undefined

call :: IO a -> ?? TMI w a

listen :: V w a -> (a -> IO ()) -> TMI w a

ex :: TMI w ()
ex = do
  v = theC root
  listen v $ \x -> msp $ "hey " ++ x
  c <- call getchar
  v <-- c
  () <- call putchar c'
  return ()

ex' :: TMI w ()
ex' = call getchar >>= (\c -> call putchar)

instance Monad (TMI w a) where
  tmi >>= k =
    let step = 
-- (>>=) :: forall a b. m a -> (a -> m b) -> m b infixl 1

runTMI :: w -> TMI w () -> IO ()
runTMI initialWorld initialAction = loop
  where loop = do
-}
