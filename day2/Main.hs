module Main where
import Data.List.Split
import Data.List
readInt :: String -> Int
readInt = read

countChar :: Char -> (String -> Int)
countChar c = length . filter (==c) 

evaluateRule :: (Int, Int, Char, String) -> Bool
evaluateRule (min, max, c, s) = countChar c s >= min && countChar c s <= max

evaluateRule2 :: (Int, Int, Char, String) -> Bool
evaluateRule2 (ind1, ind2, c, s) = (ind1 - 1 `elem` c `elemIndices` s) /= (ind2 - 1 `elem` c `elemIndices` s)

evaluateLine :: ((Int, Int, Char, String) -> Bool) -> (String -> Bool)
evaluateLine rule = (\xs -> rule(readInt (xs !! 0), readInt (xs !! 1), head (xs !! 2), xs !! 3 )) . filter (/="") . splitOneOf "- :" 


main = do 
        input <- readFile "input.txt"
        let part1 = (length . filter (evaluateLine evaluateRule)) (lines input)
        print ("part 1: " ++ show part1)
        let part2 = (length . filter (evaluateLine evaluateRule2)) (lines input)
        print ("part 2: " ++ show part2)
