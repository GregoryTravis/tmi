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

foo :: (Int, Int) -> Nester (Int, Int) (Nester Int a)
foo pr = Nester pr (Bots . fst) (\(ofst, osnd) (Bots nfst) -> (nfst, osnd))

-- ann :: Nester (Int, Int) Int
-- ann = foo (3, 4)

haha :: StateT (Nester (Int, Int) (Nester Int a)) IO (Int, Int)
haha = do
  -- x <- (nget :: Int)
  x <- nget
  npush $ do
    y <- nget
    nput $ eesp "inner" $ y + 10
    return ()
  -- TODO wow without this part, it doesn't even run most of the npush code
  --      lazy functional state threads indeed
  x' <- nget
  return x'

-- lolo = runStateT haha ann

-- runStateT :: s -> m (a, s)	
-- newtype StateT s m a

nestedStateMain = do
  let lolo = runStateT haha (foo (3, 4))
  (pr, n) <- lolo
  msp pr
  msp "nhi"
