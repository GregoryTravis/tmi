{-# LANGUAGE GADTs, PartialTypeSignatures, RankNTypes #-}

module Core
( Call(..)
, Event(..)
, Core(..)
, Program(..)
, applyContinuation
, Monitor(..)
, Monitoring(..)
, wrapAction
, Blef(..)
, toProg
, boond
, ritt
, done
, sdone
) where

import Control.Concurrent

import Util
import V
import Ty

type Monitor a = (a -> IO ())
data Monitoring w = forall a. Monitoring (V w a) (Monitor a)

data Call w = forall a. (Show a, Read a) => InternalCall String (IO a) (a -> Program w)
            | forall a. (Show a, Read a) => ExternalCall String (Int -> IO ()) (a -> Program w)
data Event w = Retval Int String | Command [String] deriving (Show, Read)
data Core w = Assign (Write w) | Call (Call w) | Sub (Program w)
            | Cond (V w Bool) (Core w) (Core w) | Named String (Core w) | Done
            | forall a. CRead (V w a) (a -> Program w)
data Program w = Program [Core w] deriving Show

instance Show (Core w) where
  show (Assign write) = "(Assign " ++ show write ++ ")"
  show (Call _) = "(Call)"
  show (Sub program) = "(Sub " ++ show program ++ ")"
  show (Cond vb _ _) = "(Cond " ++ show vb ++ ")"
  -- show (Named s c) = "(Named " ++ show s ++ " " ++ show c ++ ")"
  show Done = "(Done)"

instance Show (Call w) where
  show (InternalCall s _ _) = "(InternalCall " ++ s ++ ")"
  show (ExternalCall s _ _) = "(ExternalCall " ++ s ++ ")"

applyContinuation :: Call w -> String -> Program w
applyContinuation (InternalCall _ _ k) s = k (read s)
applyContinuation (ExternalCall _ _ k) s = k (read s)

-- Wrap an action to 'show' its value into a retval and send it down a channel.
-- Only for InternalCalls.
wrapAction :: Chan (Event w) -> Int -> Call w -> IO ()
wrapAction chan index (InternalCall _ io _) = do
  a <- io
  let as = show a
      retval = Retval index as
  -- msp $ "wrapAction chan write: " ++ show retval
  writeChan chan retval
  return ()
-- TODO: not sure we should be wrapping this, but otoh why not
wrapAction _ index (ExternalCall _ handleK _) = do
  handleK index
-- wrapAction _ _ _ = error "Cannot wrap a non-InternalCall"

---- Monad stuff

-- TODO: get rid of this existential free-ish thing and somehow just merge
-- toProg with this; then maybe we wouldn't need Show/Read on the data
-- declaration, but instead only on the actual coordination code. Then this could
-- be an actual monad instead of a QualifiedDo pseudo-monad.
data Blef w a where
  Blef :: (Read a, Show a) => String -> IO a -> Blef w a
  -- TOOD should this return Blef ()?
  EBlef :: (Read a, Show a) => String -> (Int -> IO ()) -> Blef w a
  Blefs :: forall a b w. Blef w b -> (b -> Blef w a) -> Blef w a
  BRead :: V w a -> Blef w a
  BWrite :: V w a -> a -> Blef w ()
  BFork :: (Read a, Show a) => Blef w a -> Blef w ()
  -- BCallCC :: ((a -> Blef b) -> Blef c) -> Blef c
  -- BCallCC :: ((b -> Blef w c) -> Blef w a) -> Blef w a
  -- BCallCC :: ((b -> Blef w a) -> Blef w a) -> Blef w a
  BCallCC :: (Read c, Show c) => ((a -> Blef w b) -> Blef w c) -> Blef w a
  ProgBlef :: Program w -> Blef w a
  -- BCallCC :: ((a -> Blef a) -> Blef a) -> Blef a

  -- n <- BCallCC (\krec -> Blef "" (msp "ignored")) -- krec :: (Int -> Blef String) -> Blef ()
  --                                                 -- BCallCC _ :: Blef ()


-- call/cc
-- do
--   a <- CallCC (...) -- the k-receiver here gets (a -> stuff a) :: (a -> Blef b)
--   stuff a           -- then returns some other blef which may or may not invoke the k

instance (Show a, Read a) => Show (Blef w a) where
 show (Blef s _) = "(Blef " ++ s ++ ")"
 show (Blefs b a2b) = "(Blefs)"
 show (BRead va) = "(BRead " ++ show va ++ ")"
 show (BWrite va a) = "(BWrite " ++ show va ++ ")"

boond :: Blef w a -> (a -> Blef w b) -> Blef w b
-- TODO make a constructor that only allows the correct usage pattern, infixl
boond = Blefs

ritt :: (Read a, Show a) => IO a -> Blef w a
ritt = Blef "ritt"

-- Presumably this machinery could be somehow folded in to the type so it doesn't
-- have to be free-ish.
toProg :: (a -> Program w) -> Blef w a -> Program w
toProg k (Blef s io) =
  Program [Call $ InternalCall s io k]
toProg k (EBlef s handleK) =
  Program [Call $ ExternalCall s handleK k]

toProg k (BCallCC krec) =
  let blef = krec (\a -> ProgBlef (k a))
   in toProg done blef

toProg k (Blefs blef a2Blef) =
  toProg (\a -> toProg k (a2Blef a)) blef
toProg k (BFork blef) =
  let forkedProgram = toProg done blef
      origProgram = k ()
   in Program [Sub forkedProgram, Sub origProgram]

toProg k (BRead va) =
  Program [CRead va k]
toProg k (BWrite va a) =
  Program [Assign (Write va a), Sub (k ())]

toProg k (ProgBlef p) = p

-- toProg _ x = error $ "toProg " ++ show x

-- mapCallFanIn :: V Int -> [Core W -> Core W] -> Core W -> Core W
-- mapCallFanIn counter kjobs k =
--   let n = length kjobs
--       -- countk :: () -> Core w
--       countk = Sub (Program
--         [ Assign (VWrite counter (addEm counter (VNice (1::Int))))
--         , Cond (eqV counter (VNice (n-1)))
--                k
--                Done])
--       jobs = map ($ countk) kjobs
--   in Sub (Program
--           [ Assign (Write counter 0)
--           , Sub (Program jobs)
--           ])

done :: a -> Program w
done _ = Program [Done]
sdone :: Show a => a -> Program w
sdone a = Program [Call $ InternalCall "" (msp ("sdone", a)) done]
