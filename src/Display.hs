module Display
( displayValue
, nDisplayValue ) where

import Data.List (intercalate)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Name
import Rec
import Rel
import Tuple
import Value
import Util

valuesToStrings :: [Value] -> [[String]]
valuesToStrings recs =
  let names = if null recs then [] else recNames (head recs)
      getValues map keys = [map M.! key | key <- keys]
      recToStrings (Rec _ map) = values names map
      values names map = [displayValue (map M.! key) | key <- names]
      rows = [(map displayValue (getValues mp names)) | (Rec _ mp) <- recs]
   in names : rows

-- stringses is a list of rows. Get the length of every cell, then for each
-- column get the maximum length.
displayLengths :: [[String]] -> [Int]
displayLengths stringses =
  let lengthses = map (\strings -> map length strings) stringses
   in map maximum (transpose lengthses)

-- stringses is a list of rows, lengths is the max length for each column. Pad
-- all the cells in each column to be that column's length.
padStrings :: [[String]] -> [Int] -> [[String]]
padStrings stringses lengths = map (\strings -> pad1 strings lengths) stringses
  where pad1 :: [String] -> [Int] -> [String]
        pad1 ss lens = zipWith pad2 ss lens
        pad2 s len =
          let padLength = len - (length s)
              pad = take padLength $ repeat ' '
           in s ++ pad

stringsToGridLine :: [String] -> String
stringsToGridLine ss =
  "║" ++ (intercalate "│" ss) ++ "║"

displayRel :: (S.Set Value) -> String
displayRel recs = displayGrid (valuesToStrings (S.toList recs))

-- TODO very appendy, use a builder or something
displayGrid :: [[String]] -> String
displayGrid stringses  = topHor ++ "\n" ++ (intercalate ("\n" ++ midHor ++ "\n") lines) ++ "\n" ++ botHor ++ "\n"
  where lengths = displayLengths stringses
        padded = padStrings stringses lengths
        lines = map stringsToGridLine padded
        topHor = "╔" ++ (intercalate "╤" ddashes) ++ "╗"
        midHor = "╟" ++ (intercalate "┼" dashes) ++ "╢"
        botHor = "╚" ++ (intercalate "╧" ddashes) ++ "╝"
        dashes = [take len (repeat '─') | len <- lengths]
        ddashes = [take len (repeat '═') | len <- lengths]

-- Display an nested value as a nice nested grid or whatever.
displayValue :: Value -> String
displayValue (I i) = show i
displayValue (S s) = s
displayValue (B b) = show b
displayValue r@(Rel _ recs) = displayRel recs
displayValue r@(Rec _ map) = displayValue (relFromList [r])
displayValue l@(Lst _ values) = displayGrid (map ((:[]) . displayValue) values)
displayValue (Tuple _ values) = newl $ show (map displayValue values)
displayValue a@(Adt _ name values) = newl $ paren $ intercalate " " ([name] ++ map displayValue values)
displayValue (Fun ty code) = show (ty, code)

nDisplayValue :: Value -> String
nDisplayValue v = newls (renderNVG (v2ng v))

data NestedValueGrid = Cell Value | Grid [[NestedValueGrid]]

-- Value to nested grid. Converts a nested value to a nested grid of scalars.
-- The only Cell values in the result are scalars: S, I, B.
v2ng :: Value -> NestedValueGrid
v2ng x@(I i) = Cell x
v2ng x@(S s) = Cell x
v2ng x@(B b) = Cell x
v2ng r@(Rel _ recs) = Grid (map (map v2ng) (relToGrid r))
v2ng r@(Rec _ _) = v2ng (relFromList [r])
v2ng (Lst _ values) = Grid [map v2ng values] -- hor
-- v2ng (Lst _ values) = Grid $ map ((:[]) . v2ng) values -- ver
v2ng (Tuple _ values) = Grid [map v2ng values]
v2ng a@(Adt _ name values) = Grid [[Cell (S name)] ++ map v2ng values]
v2ng (Fun _ code) = Cell (S (show code))

renderNVG :: NestedValueGrid -> [String]
renderNVG (Cell (I i)) = [show i]
renderNVG (Cell (S s)) = [s]
renderNVG (Cell (B b)) = [show b]
renderNVG (Cell x) = error ("should not have a non-scalar here: " ++ show x)
renderNVG (Grid nvgses) = renderGrid (map (map renderNVG) nvgses)

-- Equalize cells, then render to a grid.
renderGrid :: [[[String]]] -> [String]
renderGrid rows = flattenGrid (equalizeGrid rows)

-- Grid of cells where each cell is a list of strings. Find the max
-- width/height of each col/row resp and pad the cells' strings to be equal.
-- Grid is list of rows.
equalizeGrid :: [[[String]]] -> [[[String]]]
equalizeGrid rows =
  let dims = gridDims rows
      rowHeights = map (\row -> (maximum (map snd row))) dims
      rowWidths = map (\row -> (maximum (map fst row))) (transpose dims)
      maxDims = [[(w, h) | w <- rowWidths] | h <- rowHeights] :: [[(Int, Int)]]
   in zipWith (zipWith padCell) rows maxDims

-- Pad each string to be the width, and add empty lines to get to height.
padCell :: [String] -> (Int, Int) -> [String]
padCell ss (w, h) = padCellHor (padCellVer ss h) w

padCellHor :: [String] -> Int -> [String]
padCellHor ss w = map padIt ss
  where padIt s = s ++ (take (w - length s) (repeat '.'))

padCellVer :: [String] -> Int -> [String]
padCellVer ss h = ss ++ extra
  where extra = take (h - length ss) (repeat "")

gridDims :: [[[String]]] -> [[(Int, Int)]]
gridDims rows = map (map cellDim) rows

cellDim :: [String] -> (Int, Int)
cellDim ss = (maximum (map length ss), length ss)

-- Concatenate cell contents to make a single big cell, adding a border.
-- Assumes already equalized.
-- Grid is list of rows.
flattenGrid :: [[[String]]] -> [String]
flattenGrid rows =
  let flattenedRows = map flattenRow rows
      dims = gridDims rows
      widths = map fst (head dims)
      tb = topBar widths
      mb = midBar widths
      bb = botBar widths
   in [tb] ++ (intercalate [mb] flattenedRows) ++ [bb]
   --in wrap tb bb mb (map concat flattenedRows)

-- Concatenate a row of equalized cells, adding a border.
flattenRow :: [[String]] -> [String]
flattenRow cells =
  let stripes = transpose cells
      withBorder = map borderizeStripe stripes
   in withBorder

borderizeStripe :: [String] -> String
borderizeStripe ss = wrap dle dre sme ss

dul = "╔"
du = "╤"
dur = "╗"
dl = "╟"
sm = "┼"
dr = "╢"
dll = "╚"
dlow = "╧"
dlr = "╝"
dle = "║"
sme = "│"
dre = "║"

-- Build a horizontal bar out of left, right, center (divider), and filler,
-- with the given lengths.
horBar :: String -> String -> String -> String -> [Int] -> String
horBar l r c f lens = wrap l r c fills
  where fills = map (\x -> concat $ take x (repeat f)) lens

topBar = horBar dul dur du "═"
midBar = horBar dl dr sm "─"
botBar = horBar dll dlr dlow "═"

-- wrap a b c [1, 2, 3] => a1c2c3b
wrap :: String -> String -> String -> [String] -> String
wrap l r c ss = l ++ (intercalate c ss) ++ r

paren :: String -> String
paren s = "(" ++ s ++ ")"

newl :: String -> String
newl s = s ++ "\n"

-- List of strings to a single string, joined with newlines and another at the
-- end.
newls :: [String] -> String
--newls ss = newl $ intercalate "\n" ss
newls ss = concat $ map newl ss
