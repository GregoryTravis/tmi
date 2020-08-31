{-# LANGUAGE ScopedTypeVariables #-}

module Util
( assert
, assertM
, massert
, esp
, eesp
, fesp
, faresp
, faaresp
, faaresp2
, lesp
, leesp
, eeesp
, feesp
, sp
, msp
, tsp
, ttsp
, tesp
, ttesp
, fromLeftReal
, mappily
, mcompose
, time
, unsafeTime
, noBuffering
, die
, predSplit
, chomp
, replace
, weightedRandFromLists
, weightedRandFromList
, randFromList
, randFromListPure
, randFromListPureN
, clump
, allPairs
, pairUp
, whatThread
, whatThreadIO
--, minimumBy
, maximumBy
, rotate
, rotateMod
, hist
, replaceInList
, allEq
, groupUsing
, applyMaybes
, trace
, gridShow
, imap
, takeLast
, prefixes
) where

import Control.Exception
import Data.Containers.ListUtils (nubOrd)
import Data.List (group, groupBy, maximumBy, minimumBy, sort, isSuffixOf)
import qualified Data.Map.Strict as M
import Data.Text (unpack)
import Data.Text.Lazy (toStrict)
import Data.Time.Clock (diffUTCTime)
import Data.Time.Clock.System (getSystemTime, systemToUTCTime)
import Data.Typeable (typeOf)
import qualified Debug.Trace as TR
import GHC.Conc
import System.Exit (die)
import System.IO (appendFile, hFlush, stdout, stderr, hSetBuffering, BufferMode(..))
import System.IO.Unsafe
import System.Random
import Text.Pretty.Simple (pShowNoColor)
import Text.Printf

esp a = unsafePerformIO $ do
  putStrLn $ evalString $ show $ a
  return a

eesp s a = unsafePerformIO $ do
  putStrLn $ evalString $ show $ s
  hFlush stdout
  return a

fesp :: Show b => (a -> b) -> a -> a
fesp f a = unsafePerformIO $ do
  putStrLn $ evalString $ show $ f a
  return a

-- function, arg, and result
faresp :: (Show a, Show b) => (a -> b) -> (a -> b)
faresp f a = unsafePerformIO $ do
  let result = f a
  putStrLn $ evalString $ show $ (a, result)
  return result

-- thing, function, arg, and result
faaresp :: (Show a, Show b, Show c) => a -> (b -> c) -> (b -> c)
faaresp s f a = unsafePerformIO $ do
  let result = f a
  putStrLn $ evalString $ show $ (s, a, result)
  return result

faaresp2 :: (Show a, Show b, Show c, Show d) => a -> (b -> c -> d) -> (b -> c -> d)
faaresp2 s f a b = unsafePerformIO $ do
  let result = f a b
  putStrLn $ evalString $ show $ (s, a, b, result)
  return result

lesp logFile a = leesp logFile (evalString $ show a) a

leesp logFile s a = unsafePerformIO $ do
  appendFile logFile (evalString $ show s ++ "\n")
  return a

eeesp s a = unsafePerformIO $ do
  putStrLn $ evalString $ show $ (s, a)
  return a

-- Fake ones for quickly disabling
feesp s a = a

evalString s = seq (length s) s

sp x = unpack $ toStrict $ pShowNoColor $ x
--sp x = show x
msp x = putStrLn $ evalString $ sp x

tsp x = putStrLn $ (sp x) ++ " :: " ++ (sp (typeOf x))
ttsp x = putStrLn $ "_ :: " ++ (sp (typeOf x))
tesp x = eesp (sp x ++ " :: " ++ (sp (typeOf x))) x
ttesp x = eesp ("_ :: " ++ (sp (typeOf x))) x

-- Really surprised this doesn't exist
fromLeftReal (Left a) = a

massert :: Show m => m -> Bool -> IO ()
massert m b = do let _ = assert b ()
                 -- And again in case they're turned off
                 if not b
                   then throw $ AssertionFailed ("Assertion Failed: " ++ (show m))
                   else return ()

assertM :: Show b => b -> Bool -> a -> a
assertM m b a
  | b = a
  | otherwise = unsafePerformIO $ do
      putStrLn $ show m
      return $ assert b a
      --return a

-- I am giving this a terrible name because I know it must exist but I don't
-- know enough to know what it's called and I refused to accept at the moment
-- that it might be called fmap.
mappily :: (a -> b) -> Maybe a -> Maybe b
mappily f (Just x) = Just (f x)
mappily f Nothing = Nothing

mcompose :: (b -> Maybe c) -> (a -> Maybe b) -> (a -> Maybe c)
mcompose f g x = case g x of Just y -> f y
                             Nothing -> Nothing

-- Taken from https://wiki.haskell.org/Timing_computations
time :: String -> IO t -> IO t
time s a = do
    start <- getSystemTime
    v <- a
    end <- getSystemTime
    let diff = (systemToUTCTime end) `diffUTCTime` (systemToUTCTime start)
    printf "%s %s\n" s (show diff)
    return v

unsafeTime :: String -> a -> a
unsafeTime s x = unsafePerformIO (time s (return x))

noBuffering = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering

predSplit p xs = groupBy same xs
  where same a b = p a == p b

chomp :: String -> String
chomp s =
  let ('\n' : rs) = reverse s
   in reverse rs

replace a b (x : xs) | a == x = b : (replace a b xs)
replace a b [] = []

-- Sample from lists, weighting the chance of using each list by its weight
weightedRandFromLists :: [(Double, [a])] -> IO a
weightedRandFromLists weightedLists = do
  list <- weightedRandFromList weightedLists
  randFromList list

-- Pick an element from a weighted list, respecting the weights
weightedRandFromList :: [(Double, a)] -> IO a
weightedRandFromList weightedElems = do
  let weightSum = sum (map fst weightedElems)
  n <- getStdRandom (randomR (0, weightSum))
  return $ pick weightedElems n
    where pick :: [(Double, a)] -> Double -> a
          pick [] n = error "Bad pickList"
          pick ((w, x) : etc) n | n <= w = x
                                | otherwise = pick etc (n - w)

randFromList :: [a] -> IO a
randFromList [] = error "randFromList: empty list"
randFromList xs = do
  i <- getStdRandom (randomR (0, length xs - 1))
  --msp ("ORF", i >= length xs, i, length xs)
  massert ("randFromList", i, length xs) (i >=0 && i < length xs)
  return $ xs !! i

randFromListPure :: RandomGen g => g -> [a] -> (a, g)
randFromListPure g as =
  let (i, g') = randomR (0, length as - 1) g
   in (as !! i, g')

randFromListPureN :: RandomGen g => g -> [a] -> Int -> ([a], g)
randFromListPureN g as 0 = ([], g)
randFromListPureN g as n =
  let (a', g') = randFromListPure g as
      (as', g'') = randFromListPureN g' as (n-1)
   in (a':as', g'')

-- Nest elements in groups of n; ok if it doesn't divide evenly
clump :: Int -> [a] -> [[a]]
clump n [] = []
clump n xs = (take n xs) : (clump n (drop n xs))

-- Order-independent
allPairs (x:xs) = (zip (repeat x) xs) ++ allPairs xs
allPairs [] = []

-- Pair up values by matching them on the result of applying a function
pairUp :: Ord t => [a] -> [b] -> (a -> t) -> (b -> t) -> [(Maybe a, Maybe b)]
pairUp as bs aKey bKey =
  let --aMap :: M.Map t a
      aMap = M.fromList (zip (map aKey as) as)
      bMap = M.fromList (zip (map bKey bs) bs)
      allTs = nubOrd $ M.keys aMap ++ M.keys bMap
   in zip (map (aMap M.!?) allTs) (map (bMap M.!?) allTs)

whatThread :: String -> a -> a
whatThread label x = unsafePerformIO $ do
  whatThreadIO label
  return x

whatThreadIO :: String -> IO ()
whatThreadIO label = do
  threadId <- myThreadId
  (capability, pinned) <- threadCapability threadId
  msp (label, capability, pinned)

-- minimum :: Ord a => [a] -> a
-- minimum = minimumBy (<)

maximum :: Ord a => [a] -> a
maximum = maximumBy compare

-- Some monarch on SO: https://stackoverflow.com/a/55743500/5265393
-- One day I'll know why this works
rotate :: Int -> [a] -> [a]
rotate = drop <> take

rotateMod :: Int -> [a] -> [a]
rotateMod n xs = rotate n' xs
  where n' = n `mod` length xs

hist :: (Ord a) => [a] -> [(Int, a)]
hist xs = zip lens reps
  where grouped = group $ sort xs
        lens = map length grouped
        reps = map head grouped

-- What on earth is wrong with me
replaceInList :: [a] -> Int -> a -> [a]
replaceInList (x:xs) 0 x' = x' : xs
replaceInList (x:xs) n x' = x : replaceInList xs (n-1) x'
replaceInList _ _ _ = error "replaceInList"

allEq :: Eq a => [a] -> Bool
allEq [] = True
allEq (x : xs) = all (x==) xs

-- Group elements via a key (b) produced by the function
-- I want to call this groupBy but that is something else
groupUsing :: forall a b. Ord b => (a -> b) -> [a] -> [[a]]
groupUsing f xs = 
  let addIt :: a -> M.Map b [a] -> M.Map b [a]
      addIt x m =
        let k :: b
            k = f x
         in case M.lookup k m of Nothing -> M.insert k [x] m
                                 Just xs -> M.insert k (x:xs) m
      m :: M.Map b [a]
      m = foldr addIt M.empty xs
      groups :: [[a]]
      groups = M.elems m
   in groups

-- I know this exists under a standard name but I can't find it
applyMaybes :: [a -> Maybe b] -> a -> Maybe b
applyMaybes (f:fs) a = case f a of Just b -> Just b
                                   Nothing -> applyMaybes fs a
applyMaybes [] a = Nothing

trace :: Show a => a -> Bool
trace x = TR.trace (show x) False

-- lazy, and not in the good way
gridShow :: Show a => [[a]] -> [[a]]
gridShow xses = map esp xses
-- gridShow xses = unsafePerformIO $ do
--   mapM_ esp xses
--   return xses

-- Remove the suffix if it is there, otherwise return unchanged.
removeSuffix :: String -> String -> String
removeSuffix suffix s =
  if isSuffixOf suffix s
    then take (length s - length suffix) s
    else s

imap :: (Int -> a -> b) -> [a] -> [b]
imap f xs = zipWith f [0..] xs

takeLast :: Int -> [a] -> [a]
takeLast n = reverse . (take n) . reverse

prefixes :: [a] -> [[a]]
prefixes [] = []
prefixes (x:xs) = [x] : (map (x:) (prefixes xs))
