{-# LANGUAGE MultiParamTypeClasses #-}

module Day10 (main) where

filename :: FilePath
filename = "./data/day10.example1.txt"

-- filename = "./data/day10.txt"

data Tile = Vertical | Horizontal | UpRight | UpLeft | DownLeft | DownRight | Ground | Start deriving (Eq)

instance Show Tile where
  show Vertical = "|"
  show Horizontal = "-"
  show UpRight = "L"
  show UpLeft = "J"
  show DownLeft = "7"
  show DownRight = "F"
  show Ground = "."
  show Start = "S"

data Direction = Up | Down | Left | Right deriving (Eq, Show)

newtype Field = Field [[Tile]]

instance Show Field where
  show (Field field) = unlines (map showRow field)
    where
      showRow :: [Tile] -> String
      showRow = concatMap show

parseField :: [String] -> Field
parseField lines = Field [[parseTile c | c <- line] | line <- lines]
  where
    parseTile :: Char -> Tile
    parseTile c
      | c == '.' = Ground
      | c == 'S' = Start
      | c == 'L' = UpRight
      | c == 'J' = UpLeft
      | c == '7' = DownLeft
      | c == 'F' = DownRight
      | c == '|' = Vertical
      | c == '-' = Horizontal
      | otherwise = error "Unknown tile"

main :: IO ()
main = do
  ls <- lines <$> readFile filename
  let field = parseField ls
  putStrLn $ "Part 1: score: \n" ++ show field
