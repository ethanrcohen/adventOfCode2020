module Main where

import Data.List
import Data.List.Split
import Data.Maybe
import Text.Read

readMaybeInt :: String -> Maybe Int
readMaybeInt = readMaybe

readInt :: String -> Int
readInt = read

parseInput :: String -> (Int, [Maybe Int])
parseInput s = let [start, unsplt] = lines s in 
                   (readInt start, map readMaybeInt (splitOn "," unsplt))

part1 :: String -> Int
part1 s = let (earliest, maybeIds) = parseInput s; ids = catMaybes maybeIds; leaving = getEarliestTime earliest ids in
        fromJust ( find (\x -> leaving `rem` x == 0) ids) * (leaving - earliest)

getEarliestTime :: Int -> [Int] -> Int
getEarliestTime earliest = minimum . catMaybes . foldr (\id acc -> (find (>= earliest) (getArrivals id)):acc) []

getArrivals :: Int -> [Int]
getArrivals id = map (*id) [1..]

-- I just put this into wolfram alpha
part2 :: String -> [(Int, Maybe Int)] 
part2 = let (_, maybeIds) = parseInput s in 
            filter (\x(_,x) -> isJust x) (zip [0..] maybeIds)

main = do 
        input <- readFile "input.txt"
        print $ "part1: " ++ show (part1 input)
        print $ "part2: solve in wolfram alpha: " ++ show (part2 input)
