module Main where 

get2SumAndProduct :: Int -> Int -> (Int, Int)
get2SumAndProduct x y = (x+y, x*y)

get3SumAndProduct :: Int -> Int -> Int -> (Int, Int)
get3SumAndProduct x y z = (x+y+z, x*y*z)

readInt :: String -> Int
readInt = read

main = do 
        input <- readFile "input.txt"
        let nums = map readInt . words $ input
        let part1 = snd ( head ( filter (\xs -> fst xs == 2020) [get2SumAndProduct x y | x <- nums, y <- nums]))
        print ("part1: " ++ show part1)
        let part2 = snd ( head ( filter (\xs -> fst xs == 2020) [get3SumAndProduct x y z | x <- nums, y <- nums, z <- nums]))
        print ("part 2: " ++ show part2)
       
