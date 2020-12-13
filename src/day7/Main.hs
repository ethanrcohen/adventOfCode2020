module Main where

import qualified Data.Map as M
import Text.Regex.TDFA
import Data.List.Split
import Data.Maybe

-- data Bag = Bag { color :: String, components :: [(String, Int)]} deriving (Show, Eq, Ord)
type Bag = (String, [(String, Int)])

parseLine :: String -> Bag 
parseLine s = let [[_, color, components]] = s =~ "(.*) bags contain (.*)\\." :: [[String]];
                  comps = map parseComponent $ splitOn "," components in 
              (color, catMaybes comps)

parseComponent :: String -> Maybe (String, Int)
parseComponent "no other bags" = Nothing
parseComponent s = let [[_, num, color]] = s =~ "([0-9]+) (.*) bag.*" :: [[String]] in
                       Just (color, readInt num)

getAllComponents :: M.Map String [(String, Int)] -> Int ->  [(String, Int)] -> [(String, Int)]
getAllComponents m _ []  = []
getAllComponents m  multiplier  [(internalColor, count )] = let maybeComps = M.lookup internalColor m in 
                                                                   if isNothing maybeComps then [] else [(internalColor, count*multiplier)] ++ (getAllComponents m  (count * multiplier) (fromJust maybeComps) )
getAllComponents m multiplier (comp:comps) = getAllComponents m multiplier [comp] ++ getAllComponents m multiplier comps 

getBags :: String -> [Bag]
getBags s = map parseLine $ lines s

hasBagPred :: String -> [(String, Int)] -> Bool
hasBagPred color comps = any (\x -> fst x == color) comps

readInt :: String -> Int
readInt = read

part1 :: String -> Int 
part1 s = let bags = getBags s; bagMap = M.fromList bags; getComps = getAllComponents bagMap 1; shinyGoldCheck = hasBagPred "shiny gold" in
              length . filter shinyGoldCheck $ map (\x -> getComps $ snd x) bags

part2 :: String -> Int 
part2 s = let bagMap = M.fromList $ getBags s; getComps = getAllComponents bagMap 1; shinyGold = fromJust $ M.lookup "shiny gold" bagMap in
        foldr (\(_, cnt) acc -> acc + cnt) 0 $ getComps shinyGold

main = do
        input <- readFile "input.txt"
        print $ "part1: " ++ show (part1 input)
        print $ "part2: " ++ show (part2 input)
