{-# Language QualifiedDo #-}

module Parr
( parr
, parrList ) where

import Alloc
import Core
import Monad
import Ty
import Util
import V

-- call/cc at the top. Then a new contination that takes either value and
-- attempts to write it to the pair accum. You can't write it if something is there,
-- although if this code is correct that can't happen anyway. Then if both are full,
-- pass them to the main continuation.
-- Needs new Core elements: BRead (a -> Blef b) and BWrite etc.
-- Or BRead (V a)?
parr :: (Show a, Read a, Show b, Read b) =>
        V w (Alloc (Maybe a, Maybe b)) -> Blef w a -> Blef w b -> Blef w (a, b)
parr all blefa blefb = do
  Allocated acc dealloc <- alloc all (Nothing, Nothing)
  let k realK (Left a) = do
        (Nothing, myb) <- noeesp "read?" $ BRead acc
        let newP = (Just a, myb)
        BWrite acc newP
        case myb of Nothing -> return ()
                    Just _ -> case newP of (Just a, Just b) -> do dealloc; realK (a, b)
      k realK (Right b) = do
        (mya, Nothing) <- BRead acc
        let newP = (mya, Just b)
        BWrite acc newP
        case mya of Nothing -> return ()
                    Just _ -> case newP of (Just a, Just b) -> do dealloc; realK (a, b)
  BCallCC (\realK -> do
    BFork (do a <- blefa
              k realK (Left a))
    BFork (do b <- blefb
              k realK (Right b)))

parrList :: (Show a, Read a) =>
        V w (Alloc (Maybe a, Maybe [a])) -> [Blef w a] -> Blef w [a]
parrList all [] = return []
parrList all (a:as) = do
  (first, rest) <- parr all a (parrList all as)
  return (first : rest)

