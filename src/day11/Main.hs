module Main where

import Data.Maybe
import Data.List 

data State = Floor | Free | Occupied  deriving (Show, Ord, Eq, Bounded, Enum)
type Space = (Int, Int, State) 

parseState :: Char -> State
parseState c 
        | c == 'L' = Free
        | c == '#' = Occupied
        | c == '.' = Floor

parseInput :: String -> [[Space]]
parseInput s = let indexedOneD = zip [0..] (lines s) in 
               map (\(x,s) -> zipWith (\y c -> (x, y, parseState c)) [0..] s) indexedOneD

part1 :: String -> Int
part1 s = let spaces = parseInput s in
              length $ filter (\(_,_, x) -> x == Occupied) (concat $ doPart1 spaces)

doPart1 :: [[Space]] -> [[Space]]
doPart1 spaces = let newSpaces = doRound spaces in
        if (spaces == newSpaces) then spaces else doPart1 newSpaces

part2 :: String -> Int
part2 s = let spaces = parseInput s in
              length $ filter (\(_,_, x) -> x == Occupied) (concat $ doPart2 spaces)

doPart2 :: [[Space]] -> [[Space]]
doPart2 spaces = let newSpaces = doRound2 spaces in 
                     if (spaces == newSpaces) then spaces else doPart2 newSpaces

doRound2 :: [[Space]] -> [[Space]]
doRound2 state = let doSpace' = doSpace2 state in map (map doSpace') state

doRound :: [[Space]] -> [[Space]]
doRound state = let doSpace' = doSpace state in map (map doSpace') state

doSpace2 :: [[Space]] -> Space -> Space 
doSpace2 oldSpaces (x, y, state)
        | state == Floor = (x, y, state)
        | visible == 0 = (x, y, Occupied)
        | visible >= 5  = (x, y, Free)
        | otherwise = (x, y, state)
        where visible = getVisible oldSpaces x y 

doSpace :: [[Space]] ->  Space -> Space
doSpace oldSpaces (x, y, state) 
        | state == Floor = (x, y, state)
        | adjacent == 0 = (x, y, Occupied)
        | adjacent >= 4 = (x, y, Free)
        | otherwise = (x, y, state)
        where adjacent = getAdjacent oldSpaces x y 

getSeatsInDirection :: [[Space]] -> Int -> Int -> Int -> Int-> [Space]
getSeatsInDirection spaces xSlope ySlope x y = helper spaces xSlope ySlope x y x y 
        where helper :: [[Space]] -> Int -> Int -> Int -> Int -> Int -> Int -> [Space] 
              helper spaces xSlope ySlope x y firstX firstY
                 | x == firstX && y == firstY = helper spaces xSlope ySlope (x+xSlope) (y+ySlope) firstX firstY
                 | x == length spaces = [] 
                 | x < 0 = []
                 | y == length (head spaces) = []
                 | y < 0 = []
                 | otherwise = ((spaces !! x) !! y):(helper spaces xSlope ySlope (x+xSlope) (y+ySlope) firstX firstY)

getVisibleInDirection :: [[Space]] -> Int -> Int -> Int -> Int -> Int
getVisibleInDirection spaces xSlope ySlope x y = let seatsInDirection = getSeatsInDirection spaces xSlope ySlope x y 
                                                     firstSeat = find (\(_, _, state) -> state /= Floor) seatsInDirection in
                                                 if isNothing firstSeat || ( (\(_, _,state) -> state == Free) $  fromJust firstSeat) then 0 else 1

getVisible :: [[Space]] -> Int -> Int -> Int
getVisible spaces x y = let right = getVisibleInDirection spaces 1 0 x y 
                            left = getVisibleInDirection spaces (-1) 0 x y 
                            top = getVisibleInDirection spaces  0 1 x y 
                            bottom = getVisibleInDirection spaces 0 (-1) x y 
                            topRight = getVisibleInDirection spaces 1 1 x y
                            topLeft = getVisibleInDirection spaces (-1) 1 x y
                            bottomRight = getVisibleInDirection spaces 1 (-1) x y
                            bottomLeft = getVisibleInDirection spaces (-1) (-1) x y in 
                            right + left + top + bottom + topLeft + topRight + bottomLeft + bottomRight

getAdjacent :: [[Space]] -> Int -> Int ->  Int
getAdjacent spaces x y = let isOccupied = isOccupiedCurry spaces
                             maxX = (length spaces - 1)
                             maxY = (length ( head spaces) - 1)
                             right = if x == maxX then 0 else isOccupied (x+1) y
                             left = if x == 0 then 0 else isOccupied (x-1) y
                             top = if y == 0 then 0 else isOccupied x (y-1)
                             bottom = if y == maxY then 0 else isOccupied x (y+1)
                             topLeft = if y == 0 || x == 0 then 0 else isOccupied (x-1) (y-1)
                             topRight = if y == 0 || x == maxX then 0 else isOccupied (x+1) (y-1)
                             bottomLeft = if y == maxY || x == 0 then 0 else isOccupied (x-1) (y+1)
                             bottomRight = if y == maxY || x == maxX then 0 else isOccupied (x+1) (y+1) in 
                             right + left + top + bottom + topLeft + topRight + bottomLeft + bottomRight

isOccupiedCurry :: [[Space]] -> Int -> Int -> Int 
isOccupiedCurry spaces x y = let (_, _, state) = (spaces !! x) !! y in
                                 if state == Occupied then 1 else 0

main = do 
        input <- readFile "input.txt"
        print $ "part1: " ++ show (part1 input)
        print $ "part2: " ++ show (part2 input)
