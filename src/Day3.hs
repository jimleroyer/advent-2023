module Day3 (part1, part2) where

import Data.Char (isDigit)
import Data.List (foldl', intersect, nub)

filename :: FilePath
-- filename = "./data/day03.example.txt"
filename = "./data/day03.txt"

-- We won't care much about the board itself, but rather the items on the board.
-- Find out the positions of all numbers and surrounding positions of symbols.
-- Then we can find out which numbers are valid with overlapping Numbers' cells
-- and Symbols' (or filtered stars symbols) surrounding cells.
data Cell = Cell {x, y :: Int} deriving (Eq, Ord, Show)

data Number = Number {numCells :: [Cell], numValue :: Int} deriving (Show)

data Symbol = Symbol {symCell :: Cell, symValue :: Char} deriving (Show)

data BoardItem = ItemNumber Number | ItemSymbol Symbol deriving (Show)

parseBoard' :: [String] -> [BoardItem]
parseBoard' ls = concatMap (uncurry (parseLine' 0)) (zip [0 ..] ls)
  where
    parseLine' x y [] = []
    parseLine' x y wholeLine@(v : l)
      | isDigit v =
          let numberStr = greedyNumber wholeLine
              numberLen = length numberStr
              number = toInt numberStr
              cells = [x .. x + numberLen - 1] >>= \x' -> [Cell x' y]
           in ItemNumber (Number cells number) : parseLine' (x + numberLen) y (drop numberLen wholeLine)
      | v /= '.' = ItemSymbol (Symbol (Cell x y) v) : parseLine' (x + 1) y l
      | otherwise = parseLine' (x + 1) y l
    greedyNumber :: [Char] -> [Char]
    greedyNumber [] = []
    greedyNumber (x : xs)
      | isDigit x = x : greedyNumber xs
      | otherwise = []
    toInt = read :: String -> Int

class Surrounding a where
  surroundingCells :: a -> [Cell]

instance Surrounding Symbol where
  surroundingCells (Symbol (Cell x y) _) = [Cell (x + dx) (y + dy) | dx <- [-1 .. 1], dy <- [-1 .. 1], dx /= 0 || dy /= 0]

intersectingCells :: [Cell] -> [Cell] -> Bool
intersectingCells cells1 cells2 = not $ null $ cells1 `intersect` cells2

part1 :: IO ()
part1 = do
  ls <- lines <$> readFile filename
  let items = parseBoard' ls
  let symbols = [sym | ItemSymbol sym <- items]
  let symbolsCells = nub $ concatMap surroundingCells symbols
  let validNumbers = [num | ItemNumber num@(Number numberCells _) <- items, intersectingCells numberCells symbolsCells]
  let total = sum $ map numValue validNumbers
  putStrLn $ "Part 1: " ++ unlines (map show validNumbers) ++ " = " ++ show total

part2 :: IO ()
part2 = do
  ls <- lines <$> readFile filename
  let items = parseBoard' ls
  let gears = [sym | ItemSymbol sym <- items, symValue sym == '*']
  let gearsWithRatios = [(gear, [num | ItemNumber num@(Number numberCells _) <- items, intersectingCells numberCells (surroundingCells gear)]) | gear <- gears]
  let validGearsWithRatios = filter ((>= 2) . length . snd) gearsWithRatios
  let gearRatio = sum $ map (product . map numValue . snd) validGearsWithRatios
  putStrLn $ "Part 2: " ++ unlines (map show validGearsWithRatios) ++ " = " ++ show gearRatio