{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Tmi
( Val(..)
, (<--)
, oldWebDemo
, deltaTmiDemo
, bankProcess) where

import Control.Applicative
import Control.Monad.State
import qualified Data.CaseInsensitive as CI
import Data.Function
import Data.List (intersperse)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.String (IsString(..))
import qualified Data.Text as T
import Data.Text (Text)
import qualified Debug.Trace as TR
import Network.HTTP.Types.Status (ok200, found302)
import Network.URI.Encode as ENC
import System.Directory (copyFile)
import System.IO
import Text.Pretty.Simple (pShow)
import Web.Firefly

import Util 

type History = [DB]
data DB = DB { a :: Int, b :: [Int], c :: String, accounts :: M.Map String Int }
  deriving (Eq, Read, Show)

thedb = DB { a = 12, b = [2, 3, 4], c = "asdf", accounts = M.fromList [] }

data Val b = Val (DB -> b) (b -> DB -> DB)

type a --> b = Val a -> Val b
type a ---> b = Val a -> b

goo :: String --> Int
goo (Val _ _) = vconst 3
hoo :: String ---> (Int --> Float)
hoo (Val _ _) (Val _ _) = vconst 3.4

vconst v = uni $ const v

for (Val f r) = f
rev (Val f r) = r

norev = error "norev"

uni f = Val f norev

vread = for

theroot = Val id const

liftV :: (a -> b) -> Val a -> Val b
liftV f = liftBV f norev
liftV2 :: (a -> b -> c) -> Val a -> Val b -> Val c
liftV2 f = liftBV2 f norev
liftBV :: (a -> b) -> (b -> a -> a) -> Val a -> Val b
liftBV f b x = Val nf nb
  where nf db = f (for x db)
        nb ny db = rev x (b ny (for x db)) db
liftBV2 :: (a -> b -> c) -> (c -> (a, b) -> (a, b)) -> Val a -> Val b -> Val c
liftBV2 f b bbb ccc = Val fd bd
  where fd x = f (for bbb x) (for ccc x)
        bd nv x = let (nb, nc) = b nv (for bbb x, for ccc x)
                   in rev ccc nc (rev bbb nb x)

vsp v = msp $ vread v thedb

vwrite :: Val a -> Val a -> DB -> DB
vwrite (Val f b) v a = b (vread v a) a

-- bidi inc
binc :: Val Int -> Val Int
binc = liftBV (+1) (\i _ -> i-1)

-- Bidirectional additition: in the reverse direction, spread the change
-- between the two inputs.  So forward 1 + 1 = 2 ; reverse 4 = 2 + 2
bidiPlus :: Val Int -> Val Int -> Val Int
bidiPlus = liftBV2 (\x y -> x + y) rev
  where rev nsum (ox, oy) = (nx, ny)
          where osum = ox + oy
                delta = nsum - osum
                nx = ox + (delta `div` 2)
                ny = nsum - nx

instance Num a => Num (Val a) where
  (+) = liftV2 (+)
  (*) = liftV2 (*)
  abs = liftV abs
  signum = liftV signum
  fromInteger i = uni $ const $ fromInteger i
  negate = liftV negate

instance IsString a => IsString (Val a) where
  fromString s = uni $ const $ fromString s

ntrue = vconst True
nfalse = vconst False

nif :: Val Bool -> Val b -> Val b -> Val b
nif c ~t ~e = uni f
  where f db = if (for c db) then (for t db) else (for e db)

neq :: Eq b => Val b -> Val b -> Val Bool
neq = liftV2 (==)

nhead :: Val [b] -> Val b
nhead = liftBV head $ \x (_:xs) -> x:xs

ntail :: Val [b] -> Val [b]
ntail = liftBV tail $ \xs (x:_) -> x:xs

ncons :: Val b -> Val [b] -> Val [b]
ncons = liftBV2 (:) $ \(x:xs) _ -> (x, xs)

nmap :: (Eq a, Show a) => (Val a -> Val b) -> Val [a] -> Val [b]
nmap f as = nif (neq as (vconst []))
                (vconst [])
                (ncons (f (nhead as)) (nmap f (ntail as)))

nmap2 = liftV2 map

nmain = do
  hSetBuffering stdin NoBuffering
  msp "hi"
  msp $ vread theroot thedb
  --vsp theroot
  vsp $ _a
  vsp $ (liftV (+ 10)) $ _a
  vsp $ (liftV2 (+)) _a (_bi 1)
  msp $ vwrite _a 120 thedb
  massert $ (vwrite _a 120 thedb) == DB { a = 120 , b = [ 2 , 3 , 4 ] , c = "asdf", accounts = M.fromList [] } 
  vsp $ binc $ _a
  massert $ (vwrite (binc $ _a) 130 thedb) ==
    DB { a = 129 , b = [ 2 , 3 , 4 ] , c = "asdf", accounts = M.fromList [] } 
  vsp $ _a `bidiPlus` (_bi 1)
  msp $ vwrite (_a `bidiPlus` (_bi 1)) 19 thedb
  massert $ (vwrite (_a `bidiPlus` (_bi 1)) 19 thedb) ==
    DB { a = 14 , b = [ 2 , 5 , 4 ] , c = "asdf", accounts = M.fromList [] }
  let floo :: Val Int
      floo = 123
  vsp floo
  msp "mult"
  msp $ vwrite (_bi 1) 335 $ vwrite _a 126 $ vwrite _c "zxcv" thedb
  massert $ (vwrite (_bi 1) 335 $ vwrite _a 126 $ vwrite _c "zxcv" thedb) ==
    DB { a = 126 , b = [ 2 , 335 , 4 ] , c = "zxcv", accounts = M.fromList [] }
  vsp $ nmap (liftV (\x -> x * 2)) (vconst [1, 2, 3])
  vsp $ nmap2 (vconst (\x -> x * 2)) (vconst [1, 2, 3])
  massert $ (vread (nmap (liftV (\x -> x * 2)) (vconst [1, 2, 3])) thedb) == [2, 4, 6]
  massert $ (vread (nmap2 (vconst (\x -> x * 2)) (vconst [1, 2, 3])) thedb) == [2, 4, 6]
  vsp $ floo `neq` 120
  vsp $ floo `neq` 123
  vsp $ nif (vconst True) "istrue" "isfalse"
  vsp $ nif (vconst False) "istrue" "isfalse"
  vsp $ nif ntrue "istrue" "isfalse"
  vsp $ nif nfalse "istrue" "isfalse"
  vsp $ neq ntrue ntrue
  vsp $ neq ntrue nfalse
  vsp $ neq 12 12
  vsp $ neq 12 13
  vsp $ liftV (*10) 13
  vsp $ nhead (vconst [20, 21, 22])
  vsp $ ntail (vconst [20, 21, 22])
  vsp $ nhead $ ntail (vconst [20, 21, 22])
  vsp $ ncons 19 (vconst [20, 21, 22])
  vsp $ ncons 19 $ ntail (vconst [20, 21, 22])
  vsp $ (+ 10) _a
  vsp $ (+) _a (_bi 1)
  vsp $ _a + 10
  vsp $ 10 + _a
  vsp $ _a + (_bi 1)
  vsp $ nhead _b
  msp $ vwrite (nhead _b) 20 thedb
  vsp $ ntail _b
  msp $ vwrite (ntail _b) (vconst [30, 40]) thedb
  vsp $ ncons _a _b
  msp $ vwrite (ncons _a _b) (vconst [1200, 200, 300, 400]) thedb

up_a v db = db { a = v }
up_b v db = db { b = v }
up_c v db = db { c = v }
up_accounts :: M.Map String Int -> DB -> DB
up_accounts v db = db { accounts = v }
_a :: Val Int
_a = liftBV a up_a theroot
_b = liftBV b up_b theroot
_c = liftBV c up_c theroot
_accounts = liftBV accounts up_accounts theroot
_i :: Int -> Val [a] -> Val a
_i i = liftBV (!! i) (\nv oarr -> upd oarr i nv)
_m_f :: Ord a => a -> M.Map a b -> b
_m_f k m = m M.! k
_m_b :: Ord a => a -> b -> M.Map a b -> M.Map a b
_m_b k v m = M.insert k v m
_m k = liftBV (_m_f k) (_m_b k)
_keys = liftV M.keys
upd :: [a] -> Int -> a -> [a]
upd as i a
  | i < 0 || i >= length as = error "upd out of range"
  | otherwise = (take i as) ++ [a] ++ (drop (i+1) as)
--_bi i = uni $ \arr -> b arr !! i
_bi :: Int -> Val Int
_bi i = (_i i) _b

type Write = DB -> DB
mkwrite :: Val a -> Val a -> Write
mkwrite = vwrite

type TMI a = StateT [Write] IO (Val a)

infix 4 <--
(<--) :: Val a -> Val a -> TMI ()
dest <-- src = do
  writes <- get
  put $ writes ++ [mkwrite dest src]
  return $ vconst ()

applyWrites :: [Write] -> DB -> DB
applyWrites writes db = foldl (&) db writes

tmiRun :: DB -> TMI a -> IO (a, DB)
tmiRun db action = do
  msp "hi"
  (x, writes) <- runStateT action []
  let result = vread x db
      newDb = applyWrites writes db
  msp newDb
  return (result, newDb)

persistentRun :: TMI a -> IO a
persistentRun action = do
  historyS <- readFile "history.db"
  let history :: History
      history = (read historyS) :: History
  (result, newDb) <- tmiRun (last history) action
  let newHistory = history ++ [newDb]
      newHistoryS = sp newHistory
  writeFile "history.db" newHistoryS
  return result

class Delta a d where
  (.+) :: a -> d -> a
  (.-) :: a -> a -> d

data ListDelta a = Insert Int a | Delete Int
deriving instance Show a => Show (ListDelta a)

instance Delta [a] (ListDelta a) where
  xs .+ (Insert i x) = (take i xs) ++ [x] ++ (drop i xs)
  (.-) = undefined  -- slow

deltaTmiDemo = do
  msp $ ([1, 2, 3, 4] :: [Int]) .+ ((Insert 2 5) :: ListDelta Int)

processLines:: String -> (String -> IO ()) -> IO ()
processLines filename action = do
  bankCommand <- openFile filename ReadMode
  let loop = do
        eof <- hIsEOF bankCommand
        if eof
          then return ()
          else do
                  line <- hGetLine bankCommand
                  action line
                  loop
  loop

processBankCommandString :: String -> IO ()
processBankCommandString line = persistentRun $ processBankCommand (words line)

processBankCommand :: [String] -> TMI ()
processBankCommand ["createAccount", name] = do
  (_m name) _accounts <-- vconst 0
processBankCommand ["deposit", name, amount] = do
  let newBalance = (_m name) _accounts + vconst (read amount :: Int)
  (_m name) _accounts <-- newBalance
processBankCommand ["transfer", from, to, amount] = do
  (_m to) _accounts <-- (_m to) _accounts + vconst (read amount :: Int)
  (_m from) _accounts <-- (_m from) _accounts - vconst (read amount :: Int)
processBankCommand ["withdraw", from, amount] = do
  (_m from) _accounts <-- (_m from) _accounts - vconst (read amount :: Int)

bank args = do
  processBankCommand (map T.unpack args)
  return $ vconst $ WRRedirect "?q=%5B%22home%22%5D"

bankProcess = do
  --copyFile "init-history.db" "history.db"
  processLines "bank-commands.txt" processBankCommandString

bankPage :: [Text] -> WebTMI
bankPage [] = do
  let accountNames = (_keys _accounts)
  liftIO $ msp "ho"
  liftIO $ vsp accountNames
  liftIO $ vsp _accounts
  liftIO $ vsp theroot
  liftIO $ msp "ho2"
  return $ vconst $ WROk $ col [
    link "create foo" ["bank", "createAccount", "foo"],
    link "create bar" ["bank", "createAccount", "bar"],
    link "depost 100 foo" ["bank", "deposit", "foo", "100"],
    link "transfer 50" ["bank", "transfer", "foo", "bar", "50"],
    link "home" ["home"]
    ]

-- name contents attributes
data HTML = HTMLString Text | HTMLPair HTML HTML | HTMLNothing
  deriving Show
htmlRender :: HTML -> Text
htmlRender (HTMLString s) = s
htmlRender (HTMLPair a b) = (htmlRender a) `T.append` (htmlRender b)
htmlRender HTMLNothing = ""

tag :: Text -> Text -> [(Text, Text)] -> HTML
tag name contents attrs = HTMLString $ "<" <> name <> " " <> attrsS <> ">" <> contents <> "</" <> name <> ">"
  where attrsS = T.intercalate " " kevs
        kevs = [key <> "=" <> quot value | (key, value) <- attrs]
        quot s = "\"" <> s <> "\""
utag name = HTMLString $ "<" <> name <> "/>"

htmlList :: [HTML] -> HTML
htmlList htmls = mconcat htmls
col :: [HTML] -> HTML
col htmls = htmlList $ intersperse br htmls

br = utag "br"

link :: Text -> [Text] -> HTML
link text target = tag "a" text [("href", linkEncode target)]

linkEncode :: [Text] -> Text
linkEncode ss = "?q=" <> (T.pack $ ENC.encode $ show $ map T.unpack ss)

linkDecode :: Text -> [Text]
linkDecode s = read (ENC.decode $ T.unpack s)

instance Semigroup HTML where
  a <> b = HTMLPair a b

instance Monoid HTML where
  mempty = HTMLNothing

data WebResult = WROk HTML | WRRedirect String
  deriving Show

type WebTMI = TMI WebResult

instance ToResponse WebResult where
  toResponse (WROk html) = toResponse ((htmlRender $ html) :: Text, ok200, M.fromList [("Content-type", ["text/html"])] :: HeaderMap)
  toResponse (WRRedirect url) = toResponse ("" :: Text, found302, M.fromList [("Location", [T.pack url])] :: HeaderMap)

registry :: M.Map Text ([Text] -> WebTMI)
registry = M.fromList
  [ ("home", bankPage)
  , ("bank", bank)
  ]

oldWebDemo :: IO ()
oldWebDemo = run 3001 $ do
  route "/" $ do
    q <- fromMaybe defaultRoute <$> getQuery "q"
    liftIO $ msp $ linkDecode q
    let action = linkDecode q
        command:args = action
        webTmi = (registry M.! command) args
    webResult <- liftIO $ persistentRun webTmi
    liftIO $ msp webResult
    return $ webResult
  where defaultRoute = "%5B%22home%22%5D"
