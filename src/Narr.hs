{-# Language GADTs #-}

module Narr
( Narr(..)
, simulate ) where

import Util

data Narr a where
  NBind :: (Show a, Read a) => IO a -> (a -> Narr b) -> Narr b
  Done :: Narr ()

-- TODO: What do curtain-makers call this?
data Strand a = Strand (Narr a) [String]

-- TODO OPT: reverse the list?
addToLog :: Strand a -> String -> Strand a
addToLog (Strand narr log) s = Strand narr (log ++ [s])

tick :: Narr a -> String -> Narr a
tick (NBind _ k) s = k (read s)
tick Done s = error $ "Can't tick " ++ s ++ " into a Done"

runStrand :: Narr a -> [String] -> Either (Narr a) a
runStrand (NBind _ k) (s:ss) = runStrand (k (read s)) ss
runStrand n@(NBind _ _) [] = Left n
runStrand Done ss@(s:_) = error $ "Can't tick " ++ show ss ++ " into a Done"
runStrand Done [] = Right ()

simulate :: Narr a -> IO a
simulate (NBind io k) = do
  a <- io
  simulate (k a)
simulate Done = return ()
