module Main where

import Data.List
import Data.List.Split
import qualified Data.Map as M

groups :: String -> [[String]]
groups s = map lines $ splitOn "\n\n" s

countGroupAnswers :: String -> M.Map Char Int
countGroupAnswers s = M.fromListWith (+) [(c, 1) | c <- s]

part1 :: String -> Int
part1 s = sum . map (length . countGroupAnswers . concat) $ groups s

part2 :: String -> Int
part2 s = sum . map part2GroupCount $ groups s

part2GroupCount :: [String] -> Int
part2GroupCount g = let len = length g in length $ filter (\(_,cnt) -> cnt == len) (M.toList . countGroupAnswers $ concat g)

main = do
        input <- readFile "input.txt"
        print $ "part1: " ++ show (part1 input)
        print $ "part2: " ++ show (part2 input)
