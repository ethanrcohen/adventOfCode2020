module Main where

import Text.Regex.TDFA 

data Direction = E | S | W | N deriving (Show, Read, Eq, Enum, Bounded)
getDegrees :: Direction -> Int
getDegrees N = 0
getDegrees E = 90
getDegrees S = 180
getDegrees W = 270

fromDegrees :: Int -> Direction 
fromDegrees x 
                | degrees == 0 = N
                | degrees == 90 = E
                | degrees == 180 = S
                | degrees == 270 = W
                where degrees = x `mod` 360

-- for x, west is positive. for y, north is positive
type Ship = (Int, Int, Direction)


type Instruction = (Char, Int)

manhattanDistance :: Ship -> Int
manhattanDistance (x,y, _) = abs x + abs y 

readInt :: String -> Int
readInt = read

parseInstruction :: String ->  Instruction
parseInstruction s = let [[_, t, x]] = s =~ "([NSEWLRF])([0-9]+)" :: [[String]] in
                         (head t, readInt x)

part1 :: String -> Int
part1 s = let instructions = map parseInstruction $ lines s in 
              manhattanDistance (foldl doInstruction (0, 0, E) instructions)

doInstruction :: Ship -> Instruction -> Ship
doInstruction (x, y, d) (instType, insNum) = case instType of 'E' -> move (x, y, d) E insNum
                                                              'S' -> move (x, y, d) S insNum
                                                              'W' -> move (x, y, d) W insNum
                                                              'N' -> move (x, y, d) N insNum
                                                              'F' -> move (x, y, d) d insNum
                                                              'L' -> rotate (x, y, d) (-1 * insNum)
                                                              'R' -> rotate (x, y, d) insNum

move :: Ship -> Direction -> Int -> Ship
move (x, y, d) dir inc
                        | dir == E = (x + inc, y, d)
                        | dir == W = (x - inc, y, d)
                        | dir == N = (x, y + inc, d)
                        | dir == S = (x, y - inc, d)

rotate :: Ship -> Int -> Ship
rotate (x, y, d) degrees = let currentDegrees = getDegrees d in (x, y, fromDegrees (degrees +  currentDegrees))

type Waypoint = (Int, Int)
rotateWaypoint :: Waypoint -> Int -> Bool -> Waypoint
rotateWaypoint (x,y) inc clockwise  
  | inc == 0 =  (x,y)
  | inc == 180 = (-1* x, -1 *y)
  | inc == 360 =  (x, y)
  | (inc == 90 && clockwise) || (inc == 270 && not clockwise) =  (y, -1*x)
  | (inc == 270 && clockwise) || (inc == 90 && not clockwise) = (-1*y, x)


type Ship2 = (Int, Int, Waypoint)
manhattanDistance2 :: Ship2 -> Int
manhattanDistance2 (x, y, _) = abs x + abs y 


part2 :: String -> Int
part2 s = let instructions = map parseInstruction $ lines s in 
              manhattanDistance2 (foldl doInstruction2 (0, 0, (10, 1)) instructions)

doInstruction2 :: Ship2 -> Instruction -> Ship2
doInstruction2  (x, y, (wayX, wayY)) (instType, insNum) = case instType of 'E' ->  (x,y, (wayX + insNum, wayY))
                                                                           'S' ->  (x,y, (wayX, wayY - insNum))
                                                                           'W' -> (x,y, (wayX - insNum, wayY))
                                                                           'N' -> (x,y, (wayX, wayY + insNum))
                                                                           'L' -> (x, y, (rotateWaypoint (wayX, wayY) insNum False))
                                                                           'R' -> (x, y, (rotateWaypoint (wayX, wayY) insNum True))
                                                                           'F' -> ( (insNum*wayX + x), (insNum *wayY + y), (wayX, wayY))

main = do 
        input <- readFile "input.txt"
        print $ "part 1: " ++ show (part1 input)
        print $ "part2: " ++ show (part2 input)
