{-# LANGUAGE OverloadedStrings #-}

module Day2 (part1, part2) where

import Data.List (foldl', groupBy, sortOn)
import Text.Regex.TDFA
import Data.Maybe (fromJust)
import Data.Bifunctor (Bifunctor (second))

filename :: FilePath
filename = "./data/day02.txt"

zeThreshold = [("red", 12), ("green", 13), ("blue", 14)]

reGame = "Game ([0-9]+)"
reColor = "([0-9]+) ([a-zA-Z]+)"

maxValuePerColor :: [(String, Int)] -> [(String, Int)]
maxValuePerColor = map maximumBySnd . groupBy sameColor . sortOn fst
  where
    sameColor (color1, _) (color2, _) = color1 == color2
    maximumBySnd :: [(String, Int)] -> (String, Int)
    maximumBySnd = foldl1 (\acc@(color1, value1) (color2, value2) -> if value2 > value1 then (color2, value2) else acc)

parseGameNum :: String -> Int
parseGameNum line = case getAllTextSubmatches (line =~ (reGame :: String)) :: [String] of
    matches -> read (matches !! 1)

parseColors :: String -> [(String, Int)]
parseColors = matchNext []
    where
        matchNext :: [(String, Int)] -> String -> [(String, Int)]
        matchNext acc [] = acc
        matchNext acc remaining = case getAllTextSubmatches (remaining =~ (reColor :: String)) :: [String] of
            [_, num, color] ->
                let colorNum = (color, read num :: Int)
                    (index, len) = (remaining =~ (reColor :: String)) :: (Int, Int)
                    remaining' = drop (index + len) remaining
                in matchNext (colorNum : acc) remaining'

parseLine :: String -> (Int, [(String, Int)])
parseLine line = (parseGameNum line, (maxValuePerColor . parseColors) line)

filterWithThreshold :: [(Int, [(String, Int)])] -> [(String, Int)] -> [(Int, Bool)]
filterWithThreshold games threshold = map (second isAboveThreshold) games
  where
    isAboveThreshold :: [(String, Int)] -> Bool
    isAboveThreshold = all (\(color, value) -> value <= fromJust (lookup color threshold))

zePower :: [(String, Int)] -> Int
zePower colors = foldl' (*) 1 (map snd colors)

part1 :: IO ()
part1 = do
    ls <- lines <$> readFile filename
    let gamesMaxColors = map parseLine ls
    let gamesAboveThreshold = filterWithThreshold gamesMaxColors zeThreshold
    let total = sum $ map fst $ filter snd gamesAboveThreshold
    putStrLn $ "Games total: " ++ show total

part2 :: IO ()
part2 = do
    ls <- lines <$> readFile filename
    let gamesMaxColors = map parseLine ls
    let powerNumber = sum $ map (zePower . snd) gamesMaxColors
    putStrLn $ "Power number: " ++ show powerNumber
