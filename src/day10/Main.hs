module Main where 

import Data.List
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

readInt :: String -> Int
readInt = read

diff :: [Int] -> [Int]
diff [] = []
diff x = zipWith (-) (tail x) x 

getNums :: String -> [Int]
getNums s = let nums = 0:(sort . map readInt $ lines s) in  nums ++ [maximum nums + 3]

part1 :: String -> Int 
part1 s = let nums = 0:(sort . map readInt $ lines s); numsWithOurs = nums ++ [maximum nums + 3]; diffs = diff nums in 
              (length $ filter (==1) diffs) * ( length $ filter (==3) diffs)

-- part2/part2' naive approach, doesnt finish running
part2' :: [Int] -> [Int] -> [[Int]]
part2' [] (x:xs) = part2' [x] xs
part2' currentlyFormed [] = [currentlyFormed]
part2' currentlyFormed (x:xs) 
        | last - x > 3 = [[]]
        | otherwise = (part2' currentlyFormed xs) ++ (part2' (x:currentlyFormed) xs)
        where last = head currentlyFormed

part2 :: String -> Int
part2 s = let nums = getNums s; descendingNums = sortBy (flip compare) nums in 
              length $  filter (\x -> (not( null x)) && ( head x == 0)) ( part2' [] descendingNums )

part22 :: String -> Int
part22 s = let nums = getNums s; descendingNums = sortBy (flip compare) nums; numSet = IS.fromList nums; pathsMap = countPathsToAdapters numSet in 
               pathsMap IM.! 0 
        
countPathsToAdapters :: IS.IntSet -> IM.IntMap Int 
countPathsToAdapters numSet = paths 
        where device = IS.findMax numSet
              paths = flip IM.fromSet numSet  (\x -> 
                if x == device then 1 else 
                sum [IM.findWithDefault 0 (x+inc) paths | inc <- [1, 2,3]])

main = do
        input <- readFile "input.txt"
        print  $ "part1: " ++ show (part1 input)
        print $ "part2: " ++ show (part22 input)

