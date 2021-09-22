{-# LANGUAGE NamedFieldPuns #-}

module NestedState
( NestedStateT
, nestedStateMain
) where

import Control.Monad.State.Lazy

import Util

data Nester s nss = Nester s (s -> nss) (s -> nss -> s) | Bots s

-- TODO use this
type NestedStateT s ss m a = StateT (Nester s ss) m a

nget :: (MonadFail m, Monad m) => StateT (Nester s ss) m s
nget = do
  ns <- get
  return $ case ns of Nester s _ _ -> s
                      Bots s -> s

nput :: (MonadFail m, Monad m) => s -> StateT (Nester s ss) m ()
nput s' = do
  ns <- get
  case ns of Nester s f r -> put $ Nester s' f r
             Bots s -> put $ Bots s'

-- TODO write with do notation
npush :: (MonadFail m, Monad m) => StateT ss m a -> StateT (Nester s ss) m a
npush (StateT { runStateT = innerRunStateT }) = StateT { runStateT = outerRunStateT }
  where -- outerRunStateT :: (Nester s ss) -> m (a, Nester s ss)
        outerRunStateT (Nester s f r) = do
          let ss = eesp "umm" $ f s
          (a, ss') <- eesp "umm3" $ innerRunStateT ss
          let s' = eesp "umm2" r s ss'
              nester' = eesp "umm4" $ Nester s' f r
          return $ eesp "umm5" (a, nester')
        outerRunStateT (Bots _) = error "Cannot push at the bottom"

data E = E { execId :: Int, ops :: Int } deriving Show
data S = S { sops :: [String] } deriving Show

esNester :: Nester E (Nester S a)
esNester = Nester (E { execId = 12, ops = 0 }) f r
  where f _ = Bots (S { sops = [] })
        r e (Bots (S { sops })) = e { ops = (ops e) + (length sops) }

-- haha :: StateT (Nester (Int, Int) (Nester Int a)) IO (Int, Int)
haha :: StateT (Nester E (Nester S a)) IO E
haha = do
  npush $ do
    Bots (S { sops }) <- get
    put $ Bots $ S { sops = sops ++ ["a", "b"] }
  e <- nget
  return e

-- lolo = runStateT haha ann

-- runStateT :: s -> m (a, s)	
-- newtype StateT s m a

nestedStateMain = do
  (res, _) <- runStateT haha esNester
  msp res
  msp "nhi"
