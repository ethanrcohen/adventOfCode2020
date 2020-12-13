module Main where

import qualified Data.Map as M
import Data.Char
import Text.Regex.TDFA
import Data.Maybe
import Data.List

data InstructionType = JMP | ACC  | NOP deriving (Eq, Ord, Show, Read, Bounded, Enum)
readInstructionType :: String -> InstructionType
readInstructionType = read

readInt :: String -> Int
readInt = read

type Instruction = (InstructionType, Int)
readInstruction :: String -> Instruction
readInstruction s = let [[_, ins, sign, inc]] = s =~ "([a-z]+) ([\\+\\-])([0-9]+)" :: [[String]];
                        t = readInstructionType $ map toUpper ins;
                        signedInc = if sign == "+" then readInt inc else -1 * (readInt inc) in 
                        (t, signedInc)

traverse' :: M.Map Int (Instruction, Int, Bool) -> Int -> (Instruction, Int,  Bool) -> Int
traverse' _ acc (_, _, True) = acc
traverse' m acc ((t, inc), ind, False)
  | pnt >= (M.size m) && t == ACC = acc + inc
  | pnt >= (M.size m) = acc 
        | t == NOP = traverse' newM acc (fromJust $ M.lookup pnt  m)
        | t == ACC = traverse' newM (acc+inc) (fromJust $ M.lookup pnt m)
        | t == JMP = traverse' newM acc (fromJust $  M.lookup pnt m)
        where newM = M.insert ind ((t, inc), ind, True) m; pnt = getPnt (t, inc) ind

part1 :: String -> Int 
part1 s = let instructions = map readInstruction $ lines s; associationList = zip [1..] $ zip3 instructions [1..] (cycle [False]); m = M.fromList associationList; start = head associationList in 
              traverse' m 0 (snd start) 

isGood :: M.Map Int (Instruction, Int, Bool) -> (Instruction, Int, Bool) -> Bool
isGood m x = (\(_, (_, _, x)) -> x) . last . M.toList $ traverse2' m x


traverse2' :: M.Map Int (Instruction, Int, Bool) -> (Instruction, Int,  Bool) -> M.Map Int (Instruction, Int, Bool) 
traverse2' m (_, _, True) = m 
traverse2' m ((t, inc), ind, False)
        | ind == (M.size m) = newM 
        | t == NOP = traverse2' newM  (fromJust $ M.lookup pnt m)
        | t == ACC = traverse2' newM (fromJust $ M.lookup pnt  m)
        | t == JMP = traverse2' newM (fromJust $  M.lookup pnt m)
        where pnt = getPnt (t, inc) ind; newM = M.insert ind ((t, inc), ind, True) m

getPnt :: Instruction -> Int -> Int
getPnt (t, inc) ind
        | t == NOP = ind + 1
        | t == ACC = ind + 1
        | t == JMP = ind + inc

trySwap :: M.Map Int (Instruction, Int, Bool) -> [Int] -> (Instruction, Int) -> Bool
trySwap traversed goodOnes ( (t, inc), ind) = let newT = if t == NOP then JMP else NOP; newInst = (newT, inc); newM = M.insert ind (newInst, ind, False) in
        getPnt newInst ind `elem` goodOnes

part2 :: String -> Int
part2 s = let instructions = map readInstruction $ lines s 
              associationList = zip [1..] $ zip3 instructions [1..] (cycle [False]) 
              m = M.fromList associationList
              start = head associationList 
              traversed = traverse2' m (snd start)
              unvisited = M.filter (\(_, _, visited) -> not visited) traversed
              goodOnes = map fst $ filter (\x -> isGood traversed $ snd x) $ M.toList unvisited
              candidates =  M.filter (\((t,_), _, x) -> x && t /= ACC) traversed
              (badOne, badInd, _) = snd . head $ M.toList $ M.filter (\ (ins, ind, _) -> trySwap traversed goodOnes (ins, ind)) candidates
              flipped = flip' badOne
              fixedMap = M.insert badInd (flipped, badInd, False) m in
          traverse' fixedMap 0 $ snd start

flip' :: Instruction -> Instruction
flip' (t, inc) 
        | t == ACC = (t, inc)
        | t == JMP = (NOP, inc)
        | t == NOP = (JMP, inc)

main = do 
        input <- readFile "input.txt"
        print $ "part1: " ++ show (part1 input)
        print $ "part2: " ++ show (part2 input)
