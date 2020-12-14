module Main where 

import Data.List

readInt :: String -> Int
readInt = read

part1' :: [Int] -> [Int] -> [Int]
part1' _ [] = []
part1' components [x] = let sums = [a + b | a <- components, b <- (filter (/=a) components)] in 
                            if x `elem` sums then [] else [x] 
part1' components (x:xs) = let newComps = (tail components) ++ [x] in 
        part1' components [x] ++ (part1' newComps xs)


part1 :: Int -> String -> Int 
part1 count s = let nums = map readInt $ lines s; start = take count nums; rest = drop count nums in
                head ( part1' start rest)

sum' :: Int -> [Int] -> [(Int, [Int])]
sum' cnt xs 
        | len < cnt = []
        | otherwise = [(sum front, front)] ++ sum' cnt back
        where len = length xs; front = take cnt xs; back = drop 1 xs

part2 :: String -> Int
part2 s = let nums = map readInt $ lines s
              target = part1 25 s 
              sumFs = map sum' [2..]
              range = sort . snd . head . filter (\x -> fst x == target) $  concat $ map (\f -> f nums) sumFs in 
              head range +  last range


main = do 
        input <- readFile "input.txt"
        print $ "part1: " ++ show (part1 25 input)
        print $ "part2: " ++ show (part2 input)
