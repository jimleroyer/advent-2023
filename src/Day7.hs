{-# LANGUAGE MultiParamTypeClasses #-}

module Day7 (main) where

import qualified Data.Bifunctor as Bi
import Data.List

filename :: FilePath
-- filename = "./data/day07.example.txt"
filename = "./data/day07.txt"

data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine | T | J | Q | K | A deriving (Bounded, Enum, Eq, Ord, Show)

data Hand = HighCard | OnePair | TwoPair | ThreeKind | FullHouse | FourKind | FiveKind deriving (Enum, Eq, Ord, Show)

type CardAndBet = ([Card], Int)

parseLines :: [String] -> [CardAndBet]
parseLines = map (toCardWithBet . words)
  where
    toCardWithBet (hand : bet : _) = (map toCard hand, read bet :: Int)
    toCardWithBet _ = undefined
    toCard card = case card of
      '2' -> Two
      '3' -> Three
      '4' -> Four
      '5' -> Five
      '6' -> Six
      '7' -> Seven
      '8' -> Eight
      '9' -> Nine
      'T' -> T
      'J' -> J
      'Q' -> Q
      'K' -> K
      'A' -> A

getHand :: [Card] -> Hand
getHand cards = case sort (map length (group (sort cards))) of
  [1, 1, 1, 1, 1] -> HighCard
  [1, 1, 1, 2] -> OnePair
  [1, 2, 2] -> TwoPair
  [1, 1, 3] -> ThreeKind
  [2, 3] -> FullHouse
  [1, 4] -> FourKind
  [5] -> FiveKind

getJokerHand :: [Card] -> Hand
getJokerHand cards = maximum (map getHand (explode cards))
  where
    explode :: [Card] -> [[Card]]
    explode [] = [[]]
    explode (J : cs) = concatMap (\x -> map (x :) (explode cs)) [minBound .. maxBound]
    explode (c : cs) = map (c :) (explode cs)

downgradeJoker :: Card -> Int
downgradeJoker J = -1
downgradeJoker c = fromEnum c

main :: IO ()
main = do
  ls <- lines <$> readFile filename
  let cardsAndBets = parseLines ls
  let score1 = sum (zipWith (*) [1 ..] (map snd (sort (map (Bi.first (\c -> (getHand c, c))) cardsAndBets))))
  let score2 = sum (zipWith (*) [1 ..] (map snd (sort (map (Bi.first (\c -> (getJokerHand c, map downgradeJoker c))) cardsAndBets))))
  putStrLn $ "Part 1: " ++ show score1
  putStrLn $ "Part 2: " ++ show score2
