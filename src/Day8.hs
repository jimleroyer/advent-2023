{-# LANGUAGE MultiParamTypeClasses #-}

module Day8 (main) where

import qualified Data.HashMap.Strict as HM
import Data.List
import Data.Maybe (fromJust)

filename :: FilePath
-- filename = "./data/day08.example1.txt"
-- filename = "./data/day08.example2.txt"
-- filename = "./data/day08.example3.txt"
filename = "./data/day08.txt"

data Direction = L | R deriving (Enum, Eq, Ord, Show)

type Filter = (String -> Bool)

type Maze = HM.HashMap String (String, String)

parseDirections :: String -> [Direction]
parseDirections = reverse . parseLine []
  where
    parseLine directions [] = directions
    parseLine directions (d : rem) = parseLine (toDirection d : directions) rem
    toDirection 'L' = L
    toDirection 'R' = R

parseMaze :: [String] -> Maze
parseMaze lines = HM.fromList (map parsePath lines)
  where
    parsePath :: String -> (String, (String, String))
    parsePath = (\x y z -> (x, (y, z))) <$> take 3 <*> take 3 . drop 7 <*> take 3 . drop 12

nextPath :: Maze -> String -> Direction -> String
nextPath maze lastPath direction = case direction of
  L -> fst (fromJust (HM.lookup lastPath maze))
  R -> snd (fromJust (HM.lookup lastPath maze))

mkStepsCounter :: String -> Filter -> Maze -> [Direction] -> Int
mkStepsCounter start end maze directions = walk start 0 (cycle directions)
  where
    walk path steps (d : dirs)
      | end path = steps
      | otherwise = walk (nextPath maze path d) (steps + 1) dirs

countSteps1 :: Maze -> [Direction] -> Int
countSteps1 = mkStepsCounter "AAA" (== "ZZZ")

countSteps2 :: Maze -> [Direction] -> Int
countSteps2 maze directions = foldl1 lcm (map (\p -> mkStepsCounter p isEnd maze directions) starts)
  where
    starts = filter isStart (HM.keys maze)
    isStart k = k !! 2 == 'A'
    isEnd k = k !! 2 == 'Z'

main :: IO ()
main = do
  ls <- lines <$> readFile filename
  let directions = parseDirections (head ls)
  let maze = parseMaze (drop 2 ls)
  let steps1 = countSteps1 maze directions
  let steps2 = countSteps2 maze directions
  putStrLn $ "Part 1: steps: " ++ show steps1
  putStrLn $ "Part 2: steps: " ++ show steps2
