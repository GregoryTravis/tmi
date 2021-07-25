module Lift where

import V

hybrid1 :: (a -> b) -> (R a -> b -> Write) -> (R a -> R b)
hybrid1 f r ra@(R x rx) = R x' rx'
  where x' = f x
        rx' = Receiver "hybrid1" $ \x -> r ra x

hybrid2 :: (a -> b -> c) -> (R a -> R b -> c -> Write) -> (R a -> R b -> R c)
hybrid2 f r ra@(R x rx) rb@(R y ry) = R z rz
  where z = f x y
        rz = Receiver "hybrid2" $ \x -> r ra rb x

hybrid3 :: (a -> b -> c -> d) -> (R a -> R b -> R c -> d -> Write) -> (R a -> R b -> R c -> R d)
hybrid3 f r ra@(R x rx) rb@(R y ry) rc@(R z rz) = R w rw
  where w = f x y z
        rw = Receiver "hybrid2" $ \x -> r ra rb rc x
