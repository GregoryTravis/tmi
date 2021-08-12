{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import Data.Functor.Contravariant.Divisible

import Tmi
import Util

{-# ANN module "HLint: ignore Use camelCase" #-}
no_rev :: String -> a
no_rev s = error $ "No reverse for " ++ s

modString_for :: String -> String
modString_for = (++ "!")
modString_rev :: R String -> String -> Write
modString_rev (R _ rs) newS =
  rs <-- init newS
modStringV :: V (R String -> R String)
modStringV = vconst "modStringV" $ hybrid1 modString_for modString_rev

-- requests = map toRequest invitedUsersV
-- duped = mapV modStringV invitedUsersV
-- mapV :: V (R String -> R String) -> V [String] -> V [String]
-- mapV :: V (R a -> R a) -> V [a] -> V [a]
-- mapV :: V (R (a -> b) -> R [a] -> R [b])
-- mapV = undefined

-- TODO this doesn't seem right
hyGetFor1 :: (R a -> R b) -> (a -> b)
hyGetFor1 rf a =
  case rf (R a (error "whoopsie hyGetFor1")) of
    R b _ -> b
-- hyGetRev1 :: (R a -> R b) -> (R a -> b -> Write)
-- hyGetRev1 rf ra =
--   let (R _ (Receiver _ rb)) = rf ra
--    in rb

map_for :: (R a -> R b) -> [a] -> [b]
map_for rf = map (hyGetFor1 rf)
map_rev :: forall a b. R (R a -> R b) -> R [a] -> [b] -> Write
-- The goal is to map the defmodifier to the list and send it to 'ras'
-- This returns a raw (Receiver [b])
map_rev (R rf rrf) (R (oa : oas) ras) (b : bs) =
  -- Amazingly, this works too
  split (:) ras recA2w recB2w
    where recA2w ra = case rf (R oa ra) of R _ recb -> recb <-- b
          recB2w ras = map_rev (R rf rrf) (R oas ras) bs
  -- This works
  -- map_rev (R rf rrf) (R oas (Receiver "_" ras')) bs
  -- where
  --   ras' cdrAs =
  --          case rf (R oa ((:cdrAs) >$< ras)) of R _ recb -> recb <-- b
  -- This works
  -- case rf (R oa (Receiver "_" ra)) of R _ recb -> recb <-- b
  --   where ra = \a -> map_rev (R rf rrf) (R oas ((a:) >$< ras)) bs
map_rev (R rf rrf) (R [] (Receiver _ ras)) [] = ras []
map_rev _ _ _ = error "map_rev case"
-- map_rev (R rf _) (R oas (Receiver _ ras)) bs =
--   let -- foo :: [a] -> [b] -> Write
--       -- TODO not have to reverse this?
--       foo nas [] [] = ras (reverse nas)
--       foo nas (oa:oas) (b:bs) =
--         let ra = R oa (Receiver "map_rev" cont)
--             cont na = foo (na:nas) oas bs
--          in rev ra b
--       foo _ _ _ = error "unhandled in map_rev"
--       -- rev :: R a -> b -> Write
--       rev = hyGetRev1 rf
--    in foo [] oas bs
mapHy :: R (R a -> R b) -> R [a] -> R [b]
mapHy = hybrid2 map_for map_rev
mapV :: V (R (R a -> R b) -> R [a] -> R [b])
mapV = vconst "mapV" mapHy

compose_for :: (R b -> R c) -> (R a -> R b) -> (R a -> R c)
compose_for = (.)
composeHy :: R (R b -> R c) -> R (R a -> R b) -> R (R a -> R c)
composeHy = hybrid2 compose_for (no_rev "composeV")
composeV :: V (R (R b -> R c) -> R (R a -> R b) -> R (R a -> R c))
composeV = vconst "composeV" composeHy

-- -- External map
-- -- Just writing it in regular and primitive forms
-- mapE :: (a -> b) -> [a] -> [b]
-- mapE _ [] = []
-- mapE f (x:xs) = f x : (mapE f xs)
-- mapE' :: (a -> b) -> [a] -> [b]
-- mapE' f xs =
--   if null xs
--     then []
--     else (f (head xs)) : (map f (tail xs))

-- External
mapVE :: (Show a, Show b) => V (R a -> R b) -> V [a] -> V [b]
mapVE vf vas =
  ifV <**> (nullV <$$> vas)
      <**> vconst "[]" []
      <$$> (consV <**> (vf <$$> (headV <$$> vas))
          <$$> mapVE vf
          (tailV <$$> vas))

mapVE' :: (Show a, Show b) => V (R a -> b) -> V [a] -> V [b]
mapVE' vf vas =
  ifV <**> (nullV <$$> vas)
      <**> vconst "[]" []
      <$$> (consV <**> (vf <**> (headV <$$> vas))
          <$$> mapVE' vf
          (tailV <$$> vas))
  -- consV <**> (vf <**> (headV <$$> vas)) <$$> (vconst "[]" [])

zipWithV ::
  (Show a, Show b, Show c, Eq a, Eq b, Eq c) =>
  V (R a -> R b -> R c) -> V [a] -> V [b] -> V [c]
zipWithV vf vas vbs =
  ifV <**> (orV <**> (nullV <$$> vas)
                <$$> (nullV <$$> vbs))
      <**> vcheckconst "zipWith" []
      <$$> (consV <**> (vf <**> (headV <$$> vas) <$$> (headV <$$> vbs))
                  <$$> zipWithV vf (tailV <$$> vas) (tailV <$$> vbs))

-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
fyold :: (a -> b -> b) -> b -> [a] -> b
fyold f b (a:as) = fyold f (f a b) as
fyold f b [] = b
fyold' f b as =
  if null as
    then b
    else fyold' f (f (head as) b) (tail as)

foldrVE :: (Show a, Show b) => V (R a -> R b -> R b) -> V b -> V [a] -> V b
foldrVE vf vb vas =
  ifV <**> (nullV <$$> vas)
      <**> vb
      <$$> foldrVE vf
                   (vf <**> (headV <$$> vas) <$$> vb)
                   (tailV <$$> vas)

composo :: [a -> a] -> (a -> a)
composo [] = id
composo (f : fs) = composo fs . f

composo_for :: [R a -> R a] -> (R a -> R a)
composo_for = composo
composoHy :: R [R a -> R a] -> R (R a -> R a)
composoHy = hybrid1 composo_for (no_rev "composoV")
composoV :: V (R [R a -> R a] -> R (R a -> R a))
composoV = vconst "composoV" composoHy

-- TODO: why is reverseV needed here??
foldoVE :: (Show a, Show b) => V (R a -> R b -> R b) -> V b -> V [a] -> V b
foldoVE vf vb vas =
  (composoV <$$> (reverseV <$$> mapVE' vf vas)) <$$> vb

foldo :: (a -> b -> b) -> b -> [a] -> b
foldo f b as = composo (map f as) b

mapViaFold :: (a -> b) -> [a] -> [b]
--mapViaFold f as = foldr (\a bs -> f a : bs) [] as
mapViaFold f = foldr ((:) . f) []

mapViaFoldVE vf vas =
  let fooo = composeVE consV vf
   in foldoVE (vunapp fooo) (vcheckconst "mapViaFoldVE nil" []) vas

reverse_for :: [a] -> [a]
reverse_for = reverse
reverse_rev :: R [a] -> [a] -> Write
reverse_rev (R _ ra) as = ra <-- reverse as
reverseV :: V (R [a] -> R [a])
reverseV = vconst "reverseV" $ hybrid1 reverse_for reverse_rev

reverseVE :: (Eq a, Show a) => V [a] -> V [a]
reverseVE vas =
  ifV <**> (nullV <$$> vas)
      <**> vconst "[]" []
      <$$> (appendV <**> reverseVE (tailV <$$> vas)
                    <$$> (consV <**> (headV <$$> vas)
                                <$$> vcheckconst "reverseVE" []))

reverseAcc :: [a] -> [a]
reverseAcc xs = reverseAcc' xs []
  where reverseAcc' [] xs' = xs'
        reverseAcc' (x:xs) xs' = reverseAcc' xs (x:xs')

reverseAccVE :: (Eq a, Show a) => V [a] -> V [a]
reverseAccVE xs = reverseAccVE' xs (vconst "reverseAccVE" [])
  where reverseAccVE' xs xs' =
          ifV <**> (nullV <$$> xs)
              <**> xs'
              <$$> reverseAccVE' (tailV <$$> xs)
              (consV <**> (headV <$$> xs) <$$> xs')

-- Matches sizes in reverse, so array cannot change length
append_for :: [a] -> [a] -> [a]
append_for = (++)
append_rev :: R [a] -> R [a] -> [a] -> Write
append_rev (R xs rxs) (R ys rys) zs = (rxs <-- xs') <> (rys <-- ys')
  where (xs', ys') = splitAs (length xs) (length ys) zs
        splitAs len0 len1 xs = assertM "appendV: length mismatch" ok (xs', xs'')
          where xs' = take len0 xs
                xs'' = drop len0 xs
                ok = length xs' + length xs'' == length xs
appendV :: V (R [a] -> R [a] -> R [a])
appendV = vconst "appendV" $ hybrid2 append_for append_rev

inxV :: V (R [a] -> R Int -> R a)
inxV = vconst "inxV" $ hybrid2 for rev
  where for = (!!)
        rev (R as ras) (R i _) a' = ras <-- (as !!- i) a'

fstV :: V (R (a, b) -> R a)
fstV = vconst "fstV" $ hybrid1 for rev
  where for = fst
        rev (R (a, b) rab) a' = rab <-- (a', b)

sndV :: V (R (a, b) -> R b)
sndV = vconst "sndV" $ hybrid1 for rev
  where for = snd
        rev (R (a, b) rab) b' = rab <-- (a, b')

-- mapVE :: (Show a, Show b) => V (R a -> R b) -> V [a] -> V [b]
-- reverseVE :: Show a => V [a] -> V [a]
-- reverseVE vas =
--   ifV <**> (nullV <$$> vas)
--       <**> (vconst "[]" [])
--       <$$> (consV <**> (vf <$$> (headV <$$> vas))
--                   <$$> (mapVE vf
--                          (tailV <$$> vas)))

-- -- mapViaFoldVE vf vas =
-- -- foo = composoV consV (shiftNAddV <$$> 10)
-- -- • Couldn't match type ‘R [b] -> R [b]’ with ‘R c’
-- --   Expected type: V (R b -> R c)
-- --     Actual type: V (R b -> R [b] -> R [b])

-- -- works
-- foo = composeVE consV (shiftNAddV <**> vconst "" 4)
-- bar = (composeVE consV (shiftNAddV <**> vconst "" 4) (vconst "" 12)) <**> vconst "" []
-- -- baz = foldoVE <**> (composeVE consV (shiftNAddV <**> vconst "" 4))
-- baz = foldoVE (VUnPartialApp foo) (vconst "" []) -- <**> (vconst "" [3, 4])

-- -- qqq :: V (R Int) -> V (R [Int]) -> V (R [Int])
-- -- qqq = \x -> (((foo . VSeal) x) <**>) . VSeal

-- composeVE vfbc vfab va = vfbc <$$> (vfab <$$> va)
-- composeVE vfbc vfab va = vfbc <**> (vfab <**> va)
composeVE :: (Show a, Show b) => V (R a -> rest) -> V (R b -> R a) -> V b -> V rest
composeVE vfbc vfab va = vfbc <**> (vfab <$$> va)

ifV_for :: Bool -> a -> a -> a
ifV_for b t e = if b then t else e
ifV_rev :: R Bool -> R a -> R a -> a -> Write
-- ifV_rev = error "ifV_rev"
-- ifV_rev _ _  _ _ = emptyWrite
ifV_rev (R b _rb) (R t rt) (R e re) x =
  (if b then rt else re) <-- x
  -- let Receiver _ rec = if b then rt else re
  --  in rec x
ifV :: V (R Bool -> R a -> R a -> R a)
ifV = vconst "ifV" $ hybrid3 ifV_for ifV_rev

headR :: R [a] -> R a
headR (R as ras) = R a ra
  where a = head as
        ra = renameReceiver "headR" $ (:tail as) >$< ras
headV :: V (R [a] -> R a)
headV = vconst "headV" headR

tailR :: R [a] -> R [a]
tailR (R as ras) = R as' ras'
  where as' = tail as
        ras' = renameReceiver "tailR" $ (head as:) >$< ras
tailV :: V (R [a] -> R [a])
tailV = vconst "tailV" tailR

  -- divide f rb rc = Receiver "divisible" ra
  --   where ra a = case f a of (b, c) -> (rb <-- b) <> (rc <-- c)

consR :: R a -> R [a] -> R [a]
-- This compiles and runs but it should fail since it's not doing the writeIfNotNil logic
-- which I think will need Decidable.
consR (R a ra) (R as ras) = R newAs ras'
  where newAs = a:as
        ras' = divide (\(a':as') -> (a', as')) ra ras
        -- writeIfNotNil as' =
        --   if null as && null as'
        --     then emptyWrite
        --     else ras <-- as'
-- consR (R a ra) (R as ras) = R newAs ras'
--   where newAs = a:as
--         ras' = Receiver "consR" $ \(a':as') -> (ra <-- a') <> writeIfNotNil as'
--         writeIfNotNil as' =
--           if null as && null as'
--             then emptyWrite
--             else ras <-- as'
consV :: V (R a -> R [a] -> R [a])
consV = vconst "consV" consR

nullR :: R [a] -> R Bool
nullR = hybrid1 null undefined

nullV :: V (R [a] -> R Bool)
nullV = vconst "nullV" nullR

lengthV :: V (R [a] -> R Int)
lengthV = uni1 "lengthV" length

uni1 :: String -> (a -> b) -> V (R a -> R b)
uni1 n f = vconst n (hybrid1 f (no_rev n))

uni2 :: String -> (a -> b -> c) -> V (R a -> R b -> R c)
uni2 n f = vconst n (hybrid2 f (no_rev n))

bi2 :: String -> (a -> b -> c) -> (c -> (a, b)) -> V (R a -> R b -> R c)
bi2 name for rev = vconst name $ hybrid2 for rev'
  where rev' (R _ ra) (R _ rb) c =
          let (a, b) = rev c
           in (ra <-- a) <> (rb <-- b)

orV :: V (R Bool -> R Bool -> R Bool)
orV = uni2 "orV" (||)
-- orV = vconst "orV" (hybrid2 (||) (no_rev "orV"))

tup2 :: (Show a, Show b) => V (R a -> R b -> R (a, b))
-- tup2 = uni2 "tup2" (,)
tup2 = bi2 "tup2" (,) id

zipV :: (Eq a0, Eq b0, Show a0, Show b0) => V [a0] -> V [b0] -> V [(a0, b0)]
zipV = zipWithV tup2

add_hy :: Int -> R Int -> R Int
add_hy n (R x rx) = R x' rx'
  where x' = x + n
        rx' = renameReceiver "add_hy" $ (\x -> x-n) >$< rx
addV :: Int -> V (R Int -> R Int)
addV n = vconst "addV" $ add_hy n

-- inc_hy :: R Int -> R Int
-- inc_hy (R x rx) = R x' rx'
--   where x' = x + 1
--         rx' = Receiver "inc_hy" $ \x ->
--           rx <-- x - 1
-- incV :: V (R Int -> R Int)
-- incV = vconst "incV" inc_hy
incV = addV 1

plus_for :: Int -> Int -> Int
plus_for = (+)
plus_rev :: R Int -> R Int -> Int -> Write
plus_rev (R x rx) (R y ry) newZ =
  (rx <-- x') <>
  (ry <-- y')
  where x' = newZ `div` 2
        y' = newZ - x'
plus_hy' :: R Int -> R Int -> R Int
plus_hy' = hybrid2 plus_for plus_rev
plusV = vconst "plusV" plus_hy'
