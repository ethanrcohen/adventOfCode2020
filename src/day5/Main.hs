module Main  where

import Data.List
import qualified Data.Set as S

binaryPartitionWithCharacters :: (Integral a) => Char -> Char -> a -> a -> [Char] -> a 
binaryPartitionWithCharacters lowInd highInd lowBound highBound (c:cs)
                | cs == [] && c == lowInd = lowBound
                | cs == [] && c == highInd = highBound
                | c == lowInd = binaryPartitionWithCharacters lowInd highInd lowBound (highBound - inc) cs
                | otherwise = binaryPartitionWithCharacters lowInd highInd (lowBound + inc) highBound cs
                where inc =  (highBound - lowBound + 1) `div` 2

splitToVertAndHorizontal :: String -> (String, String) 
splitToVertAndHorizontal s = partition (\c -> c == 'F' || c == 'B') s

getSeat :: (String, String) -> (Int, Int)
getSeat (vert, horz) = (row, col) 
        where row = binaryPartitionWithCharacters 'F' 'B' 0 127 vert
              col = binaryPartitionWithCharacters 'L' 'R' 0 7 horz 

part1 :: String -> Int
part1 s = maximum . map ( getId  . getSeat . splitToVertAndHorizontal) $ lines s 


getId :: (Int, Int) -> Int
getId (row, col) = 8 * row + col

part2 :: String -> Int
part2 s = let seats =  map (getId . getSeat . splitToVertAndHorizontal) (lines s);
              seatRange = [minimum seats .. maximum seats];
              seatSet = S.fromList seats in
                 head ( filter (\x -> not $  x `S.member` seatSet ) seatRange )

main = do 
        input <- readFile "input.txt"
        print $ "part 1: " ++ show (part1 input)
        print $ "part 2: " ++ show (part2 input)
