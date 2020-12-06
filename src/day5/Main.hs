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

getId :: (Int, Int) -> Int
getId (row, col) = 8 * row + col

part2acc :: (Int, Int, S.Set Int) -> String -> (Int, Int, S.Set Int)
part2acc (min, max, set) s 
  | x > max = (min, x, S.union set (S.fromList [max+1 .. x-1]))
  | x < min = (x, max, S.union set (S.fromList [x+1 .. min-1]))
  | otherwise = (min, max,  (S.delete x set))
  where x = getId . getSeat . splitToVertAndHorizontal $ s

part2Folds :: String -> Int
part2Folds s = let x:xs = lines s; first = getId . getSeat . splitToVertAndHorizontal $ x in
               (\(_, _, set) -> head (S.toList set)) $ foldl part2acc (first, first, (S.fromList [])) xs

part2 :: String -> Int
part2 s = let seats =  map (getId . getSeat . splitToVertAndHorizontal) (lines s);
              seatRange = [minimum seats .. maximum seats];
              seatSet = S.fromList seats in
                 head ( filter (\x -> not $  x `S.member` seatSet ) seatRange )

part1 :: String -> Int
part1 s = maximum . map ( getId  . getSeat . splitToVertAndHorizontal) $ lines s 

main = do 
        input <- readFile "input.txt"
        print $ "part 1: " ++ show (part1 input)
        print $ "part 2: " ++ show (part2 input)
        print $ "part 2 with folds: " ++ show (part2Folds input)
