{-# LANGUAGE MultiParamTypeClasses #-}

module Day6 (part1, part2) where

import Data.List (transpose)
import Data.List.Split (splitOn)

filename :: FilePath
--filename = "./data/day06.example.txt"
filename = "./data/day06.txt"

toInt :: String -> Int
toInt s = read s :: Int

parse :: [String] -> [(Int, Int)]
parse lines = pivot numbers
  where
    numbers = map (map toInt . drop 1 . words) lines
    pivot = map (\[x, y] -> (x, y)) . transpose

parse2 :: [String] -> [(Int, Int)]
parse2 lines = numbers
  where
    numbers =  [toTuple $ map (toInt . concat . drop 1 . words) lines]
    toTuple [time, distance] = (time, distance)

getWinningRaces :: (Int, Int) -> [Int]
getWinningRaces (recordTime, distance) = filter (> distance) getRaces
  where
    range max = [0 .. max]
    getRace maxTime waitTime = (maxTime - waitTime) * waitTime
    getRaces = map (getRace recordTime) (range recordTime)

part1 :: IO ()
part1 = do
  ls <- lines <$> readFile filename
  let measures = parse ls
  let races = map getWinningRaces measures
  let score = product $ map length races
  putStrLn $ "Part 1: " ++ show score

part2 :: IO ()
part2 = do
  ls <- lines <$> readFile filename
  let measures = parse2 ls
  let races = map getWinningRaces measures
  let score = product $ map length races
  putStrLn $ "Part 2: " ++ show score