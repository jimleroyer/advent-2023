module Day4 (part1, part2) where

import Data.Char (isDigit)
import Data.List (foldl', intersect, nub)
import Data.List.Split (splitOn)
import Text.Regex.TDFA

filename :: FilePath
-- filename = "./data/day04.example.txt"
filename = "./data/day04.txt"

reGameNum = "Card +([0-9]+)"

data Card = Card {num :: Int, winningNumbers :: [Int], myNumbers :: [Int], matchingNumbers :: Int} deriving (Eq, Ord, Show)

parseCards :: [String] -> [Card]
parseCards = map parseLine
  where
    parseLine :: String -> Card
    parseLine line =
      Card
        { num = gameNum,
          winningNumbers = map read winningNumbers,
          myNumbers = map read myNumbers,
          matchingNumbers = matching
        }
      where
        [cardWinPart, myPart] = splitOn " | " line
        [cardPart, winningPart] = splitOn ": " cardWinPart
        gameNum = read $ (getAllTextSubmatches $ cardPart =~ reGameNum :: [String]) !! 1 :: Int
        winningNumbers = words winningPart
        myNumbers = words myPart
        matching = length (winningNumbers `intersect` myNumbers)

getPoints matchingNumbers
  | matchingNumbers == 0 = 0
  | otherwise = 2 ^ (matchingNumbers - 1)

trickleDown :: [Card] -> [Card]
trickleDown originalCards = trickleDown' [] originalCards
  where
    trickleDown' :: [Card] -> [Card] -> [Card]
    trickleDown' processed [] = processed
    trickleDown' processed remaining@(remCard : remCards) = trickleDown' (remCard : processed) (getCopiesFrom remCard ++ remCards)
    getCopiesFrom :: Card -> [Card]
    getCopiesFrom original = concatMap (replicate 1) nextCards
      where
        nextCardNum = num original + 1
        matching = matchingNumbers original
        nextCardsRange = [nextCardNum .. nextCardNum + matching - 1]
        nextCards = filter (\card -> num card `elem` nextCardsRange) originalCards

part1 :: IO ()
part1 = do
  ls <- lines <$> readFile filename
  let overallScore = sum $ map (getPoints . matchingNumbers) (parseCards ls)
  putStrLn $ "Part 1: " ++ unlines (map show $ parseCards ls) ++ "Overall score: " ++ show overallScore

part2 :: IO ()
part2 = do
  ls <- lines <$> readFile filename
  let copies = trickleDown $ parseCards ls
  -- putStrLn $ "Part 2: " ++ unlines (map show copies) ++ "Copies number: " ++ show (length copies)
  putStrLn $ "Part 2: Copies number: " ++ show (length copies)
