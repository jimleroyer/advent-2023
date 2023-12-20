{-# LANGUAGE MultiParamTypeClasses #-}

module Day9 (main) where

filename :: FilePath
-- filename = "./data/day09.example.txt"
filename = "./data/day09.txt"

parseReport :: [String] -> [[Int]]
parseReport lines = [[read w | w <- words line] | line <- lines]

difference :: (Num a) => [a] -> [a]
difference xs = zipWith (-) (tail xs) xs

getDiffLayers :: [[Int]] -> [Int] -> [[Int]]
getDiffLayers topLayers bottomLayer
  | null bottomLayer = topLayers
  | all (== 0) bottomLayer = topLayers ++ [bottomLayer]
  | otherwise = getDiffLayers (topLayers ++ [bottomLayer]) (difference bottomLayer)

mkValue :: (Int -> Int -> Int) -> ([Int] -> Int) -> [Int] -> Int
mkValue op position history = foldr1 op (map position (getDiffLayers [] history))

getNextValue :: [Int] -> Int
getNextValue = mkValue (+) last

getPrevValue :: [Int] -> Int
getPrevValue = mkValue (-) head

main :: IO ()
main = do
  ls <- lines <$> readFile filename
  let report = parseReport ls
  let score1 = sum (map getNextValue report)
  let score2 = sum (map getPrevValue report)
  putStrLn $ "Part 1: score: " ++ show score1
  putStrLn $ "Part 2: score: " ++ show score2
