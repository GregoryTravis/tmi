{-# LANGUAGE ExistentialQuantification, GADTs, RecordWildCards, StandaloneDeriving,
             TypeApplications, TypeFamilies #-}

module Curry (curryMain) where

import Util

data Write

-- Going for this:
-- foo :: (a -> b -> Write) -> ((a -> b) -> Write)
-- foo :: (a -> b -> c -> Write) -> ((a -> b -> c) -> Write)
--
-- With the w
-- foo :: (w -> a -> b -> Write) -> (w -> (a -> b) -> Write)
-- foo :: (a -> b -> w -> Write) -> ((a -> b) -> w -> Write)
--
-- And that old fave
-- data V (a -> b) = V (a -> b) (a <-- b)
-- data a <-- b = Lala (a -> b -> a)
-- dat a <-- b == Lala (a -> b -> a)
-- a <-- (b <-- c) == Lala (a -> (b -> c -> b) -> a)
-- (a <-- b) <-- c == Lala (a -> b -> a) -> c -> (a -> b -> a)
--
-- a -> (b, b -> a)
-- a -> (b -> (c, c -> b), (b -> (c, c -> b)) -> a)
-- a -> (b -> (c, c -> (a, b)))

-- TODO: no, a V can't just have an a, should at least be w -> a
data V a = V a (a -> Write)

-- instance Applicative V where
-- instance Functor VV where
--   fmap f (VV wa) = VV f'
--     where f' w = f (wa w)
-- 
-- instance Applicative VV where
--   pure x = VV (\_ -> x)
--   (VV wf) <*> (VV wx) = VV (\w -> (wf w) (wx w))

data F1 a b = F1 (a -> b) (a -> b -> Write)

luft1 :: F1 a b -> V (a -> b)
--    :: F1 (a -> b) (a -> b -> Write) -> V (a -> b) ((a -> b) -> Write)
luft1 (F1 f r) = V f (luftRev1 r)

luftRev1 :: (a -> b -> Write) -> ((a -> b) -> Write)
-- also  :: (a -> b -> Write) ->  (a -> b) -> Write 
luftRev1 rev huh = undefined

-- applicative-ish
appy :: V (a -> b) -> V a -> V b
--   :: V (a -> b) ((a -> b) -> Write) ->
--      V a (a -> Write) ->
--      V b (b -> Write)
appy = undefined

curryMain = do
  msp "curry hi"
