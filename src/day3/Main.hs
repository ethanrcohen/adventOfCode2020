module Main where 

accumulatorWithSlope :: Int -> (Int, Int) -> String -> (Int, Int)
accumulatorWithSlope x (sum, lastInd) row = if ((cycle row) !! (lastInd + x)) == '#' then (sum+1, lastInd+x) else (sum, lastInd+x)

findTreesWithSlope :: (Int, Int) -> String -> Int
findTreesWithSlope (x, y) s = fst . foldl (accumulatorWithSlope x) (0, -1*x) . filterToEveryNthEntry y $ lines s

filterToEveryNthEntry :: Int -> [a] -> [a]
filterToEveryNthEntry n xs = map snd $ filter (\(i, _) -> i `mod` n == 0) $ zip [0..] xs

main = do 
        input <- readFile "input.txt" 
        let part1 = findTreesWithSlope (3,1) input
        print ( "part1 : " ++ show part1)
        let part2 = part1 * (findTreesWithSlope (1,1) input) * (findTreesWithSlope (7,1) input) * (findTreesWithSlope (5,1) input) * (findTreesWithSlope (1,2) input) 
        print ( "part 2: " ++ show part2)
