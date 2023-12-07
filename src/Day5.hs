{-# LANGUAGE MultiParamTypeClasses #-}

module Day5 (part1, part2) where

import Control.Arrow (Arrow (first))
import Data.Char (isDigit)
import Data.IntervalMap.Generic.Interval (Interval (lowerBound))
import qualified Data.IntervalMap.Generic.Strict as IM
import Data.List (foldl', intersect, nub)
import Data.List.Split (splitOn)
import Text.Regex.TDFA

filename :: FilePath
-- filename = "./data/day05.example.txt"
filename = "./data/day05.txt"

type IntervalLength = Int

type IntervalStart = Int

type IntervalEnd = Int

data MyInterval = MyInterval {low :: IntervalStart, high :: IntervalEnd} deriving (Eq, Ord, Show)

createInterval :: IntervalStart -> IntervalLength -> MyInterval
createInterval low range = MyInterval {low = low, high = low + range}

instance IM.Interval MyInterval Int where
  lowerBound = low
  upperBound = high
  rightClosed _ = False

newtype Seed = Seed {seedId :: Int} deriving (Eq, Ord, Show)

data AlmanacMap = AlmanacEntry {srcRange :: MyInterval, destRange :: MyInterval} deriving (Eq, Ord, Show)

toInt w = read w :: Int

splitIntoSections :: String -> [String]
splitIntoSections = splitOn "\n\n"

parseSeeds :: String -> [Seed]
parseSeeds line = map (toSeed . toInt) seedNumbers
  where
    seedNumbers = drop 1 (words line)
    toSeed i = Seed {seedId = i}

parseSeedRanges :: String -> [Seed]
parseSeedRanges line = seeds
  where
    numbers = map toInt (drop 1 (words line))
    pairs = zip (evenIndices numbers) (oddIndices numbers)
      where
        evenIndices xs = [x | (x, i) <- zip xs [0..], even i]
        oddIndices xs = [x | (x, i) <- zip xs [0..], odd i]
    toSeed i = Seed {seedId = i}
    seeds = concatMap (\(seedId, range) -> map toSeed [seedId .. (seedId + range - 1)]) pairs

parseAlmanacEntries :: [String] -> [AlmanacMap]
parseAlmanacEntries lines = parseLines lines []
  where
    parseLines :: [String] -> [AlmanacMap] -> [AlmanacMap]
    parseLines [] almanacs = reverse almanacs
    parseLines (lines@line : rem) almanacs = parseLines rem (toAlmanac line : almanacs)
      where
        toAlmanac line =
          AlmanacEntry
            { srcRange = createInterval srcLow mapRange,
              destRange = createInterval destLow mapRange
            }
          where
            numbers = map toInt (words line)
            destLow = head numbers
            srcLow = numbers !! 1
            mapRange = numbers !! 2

getNearestLocation :: [[AlmanacMap]] -> [Seed] -> [Int]
getNearestLocation almanacs seeds = getNearestLocation' almanacs (map seedId seeds)
  where
    getNearestLocation' :: [[AlmanacMap]] -> [Int] -> [Int]
    getNearestLocation' [] params = params
    getNearestLocation' (currentMapping : mappings) params = getNearestLocation' mappings newParams
      where
        -- newParams =
          -- [ let filtered = [c | m <- currentMapping, let c = correlate m p, c /= p]
          --    in if null filtered then p else head filtered
          --   | p <- params
          -- ]
        correlations = [[correlate m p | m <- currentMapping] | p <- params]
        newParams = [let filtered = [c | c <- cs, c /= p] 
                    in if null filtered then p else head filtered | (p, cs) <- zip params correlations]
        correlate :: AlmanacMap -> Int -> Int
        correlate mapping param
          | IM.inside param (srcRange mapping) = getNextValue mapping param
          | otherwise = param
        getNextValue almana param = lowerBound (destRange almana) - lowerBound (srcRange almana) + param

part1 :: IO ()
part1 = do
  ls <- lines <$> readFile filename
  let seeds = parseSeeds (head ls)
  let sections = map (drop 1 . lines) (splitIntoSections $ unlines (drop 2 ls))
  let almanacs = map parseAlmanacEntries sections
  let locations = getNearestLocation almanacs seeds
  putStrLn $ "Part 1: " ++ show (minimum locations)

part2 :: IO ()
part2 = do
  ls <- lines <$> readFile filename
  let seeds = parseSeedRanges (head ls)
  let sections = map (drop 1 . lines) (splitIntoSections $ unlines (drop 2 ls))
  let almanacs = map parseAlmanacEntries sections
  let locations = getNearestLocation almanacs seeds
  putStrLn $ "Part 2: " ++ show (minimum locations) 
  -- Start: 6:35:39
