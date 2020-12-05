module Main where 
       
import Data.List 
import Data.Function
import Data.List.Split
import qualified Data.Map as M 

data Passport = Passport {
                        byr :: String,
                        iyr :: String,
                        eyr :: String,
                        hgt :: String,
                        hcl :: String,
                        ecl :: String,
                        pid :: String,
                        cid :: Maybe String
                         } deriving (Show)

--parseInput :: String -> [Passport]
-- parseInput s = map concat (filter (/=[""]) $ groupBy ((==) `on` (/="")) $ lines input)

splitToAssociationListOnDelimiter :: String -> String -> [(String, String)]
splitToAssociationListOnDelimiter delimiter s = map (\x -> let a = splitOn delimiter x in (a !! 0, a !! 1)) $ words s

numberOfLegalPassports :: String -> Int
numberOfLegalPassports s = length . filter (\x -> M.size x == 8 || (M.size x == 7 && not (M.member "cid" x)) ) $  map (M.fromList . splitToAssociationListOnDelimiter ":")  $ splitOn "\n\n" s

main = do
        input <- readFile "input.txt"
        let part1 = numberOfLegalPassports input
        print $ "part 1:" ++ show part1
--        map (m.FromList . splitToAssociationList . concat) (filter (/=[""]) $ groupBy ((==) `on` (/="")) $ lines input)
