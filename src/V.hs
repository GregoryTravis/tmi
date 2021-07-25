module V where

-- data Write1 = forall a. Write1 a
data Write1 = forall a. Show a => Write1 (V a) a
deriving instance Show Write1
data Write = Write [Write1] deriving Show
emptyWrite :: Write
emptyWrite = Write []
instance Semigroup Write where
  Write ws <> Write ws' = Write $ ws ++ ws'
data Receiver a = Receiver String (a -> Write)
instance Show (Receiver a) where
  show (Receiver s _) = "REC Receiver " ++ s
-- data Receiver a = Receiver (V a)
data R a = R a (Receiver a)
  deriving Show
infix 1 <--
(<--) :: Receiver a -> a -> Write
Receiver s r <-- x = {-eeesp ("REC <-- call", s) $-} r x
-- Receiver va <-- x = Write [Write1 va x]
-- Receiver va <-- x = Write [Write1 va x]

data V a where
  VRoot :: V a
  VConst :: (Show a) => String -> a -> V a
  VCheckConst :: (Show a, Eq a) => String -> a -> V a
  VPartialApp :: (Show a) => V (R a -> rest) -> V a -> V rest
  VUnPartialApp :: (Show a) => (V a -> V rest) -> V (R a -> rest)
  VApp :: (Show a, Show b) => V (R b -> R a) -> V b -> V a
  VSeal :: (Show a) => V (R a) -> V a
  -- VUnSeal :: (Show a) => V a -> V (R a)

-- more succinct
k :: (Show a) => String -> a -> V a
k = VConst
infixl 4 <**>
(<**>) :: (Show a) => V (R a -> rest) -> V a -> V rest
(<**>) = VPartialApp
infixl 4 <$$>
(<$$>) :: (Show a, Show b) => V (R b -> R a) -> V b -> V a
(<$$>) = VApp

instance Show a => Show (V a) where
  show VRoot = "[root]"
  show (VConst s a) = "(VConst " ++ s ++ " " ++ show a ++ ")"
  -- TODO: Replace VConst with this, and add Eq to everything
  show (VCheckConst s a) = "(VCheckConst " ++ s ++ " " ++ show a ++ ")"
  -- show (VApp vfba vfb) = "(" ++ (show vfba) ++ " " ++ (show vfb) ++ ")"
  -- show (VPartialApp vf va) = "(" ++ (show vf) ++ " " ++ (show va) ++ ")"
  show (VApp vfba vfb) = "(" ++ (show vfba) ++ " " ++ "arg" ++ ")"
  show (VPartialApp vf va) = "(" ++ (show vf) ++ " " ++ "arg" ++ ")"
  show (VSeal va) = "(seal " ++ (show va) ++ ")"

r :: History w -> V a -> a
-- r :: W -> V a -> a
r (History (w:_)) VRoot = unsafeCoerce w
r _ (VConst _ x) = x
r _ (VCheckConst _ x) = x
r h (VApp vfbfa vb) = r h (VSeal (VPartialApp vfbfa vb))
-- TODO not crazy about constructing receivers here
-- r w (VApp vf va) = b
--   where f = r w vf
--         a = r w va
--         -- rb = R b (Receiver $ \b' -> Write [Write1 b'])
--         ra = R a (Receiver "r VApp" $ \a' -> Write [Write1 va a'])
--         rb = f ra
--         b = case rb of R b _ -> b
r h (VSeal vra) = a
  where ra = r h vra
        a = case ra of R a _ -> a
r h (VPartialApp vf va) = paf
  where f = r h vf
        a = r h va
        ra = R a (Receiver "r VPartialApp" $ \a' -> Write [Write1 va a'])
        paf = f ra
-- VUnPartialApp :: (Show a) => (V a -> V rest) -> V (R a -> rest)
-- must return (R a -> rest)
r h (VUnPartialApp vvf) = \ra -> ((r h (vvf (VSeal (VConst "uh" ra)))))
-- r h (VUnPartialApp vvf) = \ra ->     -- ra = -- :: rest
--   let va = VConst "VUnPartialApp" ra
--       vRest = vvf va
--       rest = r h vRest
--    in rest

wr :: History w -> V a -> a -> Write
-- wr :: W -> V a -> a -> Write
-- wr w VRoot _ = undefined "Can't write to root"
wr h v@(VConst s _) _ = error $ "Can't write to a const: " ++ s ++ " " ++ (show v)
wr h v@(VCheckConst s x) x'
  | x == x' = emptyWrite
  | otherwise = error $ "VCheckConst: unequal: " ++ show v ++ " <-- " ++ show x'
-- This was just to ignore what I figured was a equi-const write
-- wr h (VConst s _) _ = emptyWrite
-- Can't
-- wr h (VConst s x) x'
--   | x == x' = error "but ok"
--   | otherwise = error $ "Can't write to a const: " ++ s
wr h (VApp vfbfa vb) b = wr h (VSeal (VPartialApp vfbfa vb)) b
  --where -- write = Write [Write1 vb b']
  --      write = reca a
  --      rbra = r w vfbfa
  --      -- ra = rbra rb
  --      R _ (Receiver reca) = rbra rb
  --      rb = R b (Receiver $ \b' -> Write [Write1 vb b'])
  --      b = r w vb
  --      --b' = undefined
-- Good gravy why is this not needed?
wr h (VPartialApp vf va) _ = error "Why is this error not happening"
wr h (VSeal vra) a = write
  where write = {-eeesp ("REC wr2", s) $-} reca a
        R _ (Receiver s reca) = ra
        ra = r h vra

