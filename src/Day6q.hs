{-# LANGUAGE MultiParamTypeClasses #-}

module Day6q (part1, part2) where

import Data.List (transpose)
import Data.List.Split (splitOn)

filename :: FilePath
-- filename = "./data/day06.example.txt"
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
    numbers = [toTuple $ map (toInt . concat . drop 1 . words) lines]
    toTuple [time, distance] = (time - 1, distance)

functionRoot :: (Ord b, Floating b, RealFrac b) => b -> b -> b -> (Int, Int)
functionRoot a b c = if d < 0 then error "0" else (floatToInt x, floatToInt y)
  where
    x = e + sqrt d / (2 * a)
    y = e - sqrt d / (2 * a)
    d = b * b - 4 * a * c
    e = -b / (2 * a)
    floatToInt f
      | f == fromIntegral (round f) = round (f + 1.0)
      | f < fromIntegral (round f) = ceiling f
      | otherwise = floor f

getWinningNumber :: (Int, Int) -> Int
getWinningNumber (recordTime, distance) = y - x
  where
    a = fromIntegral (-1)
    b = fromIntegral recordTime
    c = fromIntegral (-distance)
    (x, y) = functionRoot a b c

part1 :: IO ()
part1 = do
  ls <- lines <$> readFile filename
  let measures = parse ls
  let score = product $ map getWinningNumber measures
  putStrLn $ "Part 1: " ++ show score

part2 :: IO ()
part2 = do
  ls <- lines <$> readFile filename
  let measures = parse2 ls
  let score = product $ map getWinningNumber measures
  putStrLn $ "Part 2: " ++ show score