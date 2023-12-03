module Day1s (part2) where

import Data.Char (toLower)
import Text.Regex.TDFA ((=~))

filename = "./data/day01.txt"
pack (a, b) = read $ show a ++ show b
readInt :: (Num a, Read a) => [Char] -> a
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

searchFirst = search id
searchLast = search reverse
firstAndLast input = pack (searchFirst input, searchLast input)

part2 :: IO ()
part2 = do
    lines <- lines <$> readFile filename
    let total = sum $ map firstAndLast lines
    putStrLn $ "Sum: " ++ show total
