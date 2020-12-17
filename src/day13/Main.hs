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

main = do 
        input <- readFile "input.txt"
        print "foo"
