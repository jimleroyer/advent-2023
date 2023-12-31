{-# LANGUAGE OverloadedStrings #-}

module Day1 (part1, part2) where

import Data.Char (digitToInt, isDigit, toLower)
import Data.List (head, last)
import Text.Regex.TDFA ((=~))

filename :: FilePath
filename = "./data/day01.txt"

nums :: String -> [Int]
nums = map digitToInt . filter isDigit

pack :: (Int, Int) -> Int
pack (a, b) = read $ show a ++ show b

firstAndLast :: [Int] -> Int
firstAndLast arr = pack (head arr, last arr)

readInt :: String -> Int
readInt word = case map toLower word of
    "one" -> 1
    "two" -> 2
    "three" -> 3
    "four" -> 4
    "five" -> 5
    "six" -> 6
    "seven" -> 7
    "eight" -> 8
    "nine" -> 9
    _ -> read word

reNumbers = "one|two|three|four|five|six|seven|eight|nine"

search :: (String -> String) -> String -> Int
search transform input = case (transform input =~ ("[0-9]|" ++ transform reNumbers :: String) :: String) of
  [] -> 0
  match -> readInt $ transform match

searchFirst :: String -> Int
searchFirst = search id

searchLast :: String -> Int
searchLast = search reverse

firstAndLast' :: String -> Int
firstAndLast' input = pack (searchFirst input, searchLast input)

part1 :: IO ()
part1 = do
    lines <- lines <$> readFile filename
    let total = sum $ map (firstAndLast . nums) lines
    putStrLn $ "Sum: " ++ show total

part2 :: IO ()
part2 = do
    lines <- lines <$> readFile filename
    let total = sum $ map firstAndLast' lines
    putStrLn $ "Sum: " ++ show total
