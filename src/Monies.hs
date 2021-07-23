{-# LANGUAGE
    BlockArguments
  , ExistentialQuantification
  #-}

module Monies
( moniesMain ) where

import Control.Applicative
import Control.Monad (join)

import Util

newtype B a = B a deriving Show

instance Functor B where
  fmap f (B a) = B (f a)

instance Applicative B where
  pure = B
  liftA2 f (B a) (B b) = B (f a b)
-- liftA2 f x = (<*>) (fmap f x)
-- (<*>) = liftA2 id

instance Monad B where 
  B a >>= aToBb = aToBb a
  -- Hangs, why?
  -- B a >>= aToBb = join $ fmap aToBb (B a)
  -- B a >>= aToBb = fmap aToBb (B a)
-- (>>=) :: forall a b. m a -> (a -> m b) -> m b infixl 1

data Fri a = Ret a | forall c. Bund (Fri c) (c -> Fri a)

instance Functor Fri where
  fmap f (Ret a) = Ret (f a)
  fmap f (Bund fc cfa) = Bund fc (fmap f . cfa)

instance Applicative Fri where
  pure = Ret
  -- liftA2 f fa fb = fmap (\bc -> fmap bc fb) (fmap f fa) -- Fri (b -> c)
  -- liftA2 f fa fb = fmap (\b2c -> fmap b2c fb) (fmap f fa) -- Fri (b -> c)
  -- liftA2 f fa fb = liftA2 id (fmap f fa) fb
  -- redundant? No, it hung
  liftA2 f (Ret a) (Ret b) = Ret $ f a b 
  liftA2 f (Ret a) (Bund fx xToFb) = Bund fx (fmap (f a) . xToFb)
  liftA2 f (Bund fx xToFa) fb = Bund fx (\x -> fmap f (xToFa x) <*> fb)

instance Monad Fri where
  return = Ret
  (>>=) = Bund

evl :: Fri a -> a
evl (Ret a) = a
evl (Bund fb bToFa) = evl (bToFa (evl fb))

moniesMain = do
  msp $ fmap (+1) (B 12)
  msp $ liftA2 (+) (B 12) (B 13)
  msp $ B 1 >>= \i -> B (i*10)
  msp $ B 1 >>= B . (*100)
  msp $ evl $ Ret 12
  msp $ evl $ do return 12
  msp $ evl $ Ret 12 >>= Ret . (*20)
  msp $ evl $ do 
                i <- return 12
                return $ (i*20)
  msp $ evl $ liftA2 (+) (Ret 20) (Ret 300)
  msp "hi monies"
