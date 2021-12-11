module C (cMain) where

import Util

foo :: Int -> Int
foo x = x * 2

fooK :: Int -> (Int -> r) -> r
fooK x k = k (x * 2)
fooK' :: Int -> (Int -> r) -> r
fooK' = klift foo

klift :: (a -> b) -> (a -> (b -> r) -> r)
klift f = \x k -> k (f x)

-- here's the method
-- a in double negative position means you can magically produce an a inside an application
-- foo :: (a -> b) -> c
-- foo a2b = a2b (\a -> ... use a to produce c)

-- (>>=) :: forall a b. m a -> (a -> m b) -> m b
data C r a = C ((a -> r) -> r)
-- ((a -> r) -> r) >>= (a -> (b -> r) -> r) :: C r b
boond :: ((a -> r) -> r) -> (a -> (b -> r) -> r) -> ((b -> r) -> r)
boond kfa a2kfb = \b2r -> kfa ((flip a2kfb) b2r)

boondC :: C r a -> (a -> C r b) -> C r b
-- yikes
-- boondC (C kfa) a2Crb = C (\b2r -> kfa (\a -> (case (a2Crb a) of C br2r -> br2r) b2r))
-- still yikes
boondC (C kfa) a2Crb = C foo
  where foo b2r = kfa bar
          where bar a = -- (case (a2Crb a) of C br2r -> br2r) b2r
                  let C br2r = a2Crb a
                   in br2r b2r

-- fmap :: (a -> b) -> f a -> f b
-- fmap :: (a -> b) -> C ((a -> r) -> r) -> C ((b -> r) -> r)
-- so much yikes
instance Functor (C r) where
  fmap ab (C ar2r) = C br2r
    where br2r b2r = ar2r (\a -> b2r (ab a))

-- pure :: a -> f a
-- (<*>) :: f (a -> b) -> f a -> f b 
-- (<*>) :: C r (a -> b) -> C r a -> C r b 
-- (<*>) :: C (((a -> b) -> r) -> r) -- a2b2r2r
--       -> C ((a -> r) -> r)        -- a2r2r
--       -> C ((b -> r) -> r)        -- b2r2r
-- yikes out the wazoo
instance Applicative (C r) where
  pure x = C (\k -> k x)
  C a2b2r2r <*> C a2r2r =
    let b2r2r b2r =  -- return an r
          a2b2r2r (\a2b -> a2r2r (\a -> b2r (a2b a)))
          -- a2b2r2r (\a2b2r -> a2b2r (\a2b -> a2r2r (\a -> b2r (a2b a))))
     in C b2r2r

instance Monad (C r) where
  (>>=) = boondC

cMain = do
  msp $ fooK 12 (\x -> show $ "yep " ++ show x)
  msp $ fooK' 13 (\x -> show $ "yep " ++ show x)

