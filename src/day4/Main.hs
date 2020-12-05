module Main where 

import Data.Char
import Data.List 
import Data.Function
import Data.List.Split
import qualified Data.Map as M 
import qualified Data.Set as S

data Passport = Passport {
                        byr :: Maybe String,
                        iyr :: Maybe String,
                        eyr :: Maybe String,
                        hgt :: Maybe String,
                        hcl :: Maybe String,
                        ecl :: Maybe String,
                        pid :: Maybe String,
                        cid :: Maybe String
                         } deriving (Show)

readInt :: String -> Int 
readInt = read

readIntMaybe :: Maybe String -> Maybe Int
readIntMaybe Nothing = Nothing
readIntMaybe (Just s) = (Just $ readInt s)

splitToAssociationListOnDelimiter :: String -> String -> [(String, String)]
splitToAssociationListOnDelimiter delimiter s = map (\x -> let a = splitOn delimiter x in (a !! 0, a !! 1)) $ words s

numberOfLegalPassports :: (Passport -> Bool) -> String -> Int
numberOfLegalPassports f s = length . filter f $ map passportFromValidMap $ formPassportMapList s

formPassportMapList :: String -> [M.Map String String]
formPassportMapList s = filter (\x -> M.size x == 8 || (M.size x == 7 && not (M.member "cid" x)) ) $  map (M.fromList . splitToAssociationListOnDelimiter ":")  $ splitOn "\n\n" s

passportFromValidMap :: M.Map String String -> Passport
passportFromValidMap m = Passport (M.lookup "byr" m) (M.lookup "iyr" m) (M.lookup "eyr" m) (M.lookup "hgt" m ) (M.lookup "hcl" m) (M.lookup "ecl" m) (M.lookup "pid" m) (M.lookup "cid" m)

maybeCompareTo :: (Ord a) => (a -> a -> Bool) -> a -> Maybe a -> Bool
maybeCompareTo _ _ Nothing = False
maybeCompareTo f x (Just y) = f x y

maybeBetweenInclusive :: (Ord a) => a -> a -> Maybe a -> Bool
maybeBetweenInclusive low hi m = (maybeCompareTo (<=) low m) && (maybeCompareTo (>=) hi m)

validateHeight :: (Maybe String) -> Bool
validateHeight Nothing = False
validateHeight (Just s) = let xs = partition isNumber s; num = readInt $ fst xs; unit = snd xs in if (unit == "cm") then (num >= 150 && num <= 193) else (num >= 59 && num <= 76)

validateHcl :: (Maybe String) -> Bool
validateHcl Nothing = False
validateHcl (Just s) = (head s == '#') && (length s == 7) && (all (\c -> isNumber c ||  c `S.member` validLetters) $ tail s) where validLetters = S.fromList ['a' .. 'f']

validateEcl :: (Maybe String) -> Bool
validateEcl Nothing = False
validateEcl (Just s) = s `S.member` validEyeColors where validEyeColors = S.fromList ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

validatePid :: (Maybe String) -> Bool
validatePid Nothing = False
validatePid (Just s) = all isNumber s  &&  length s == 9

partTwoValidation :: Passport -> Bool
partTwoValidation p = (maybeBetweenInclusive 1920 2002 (readIntMaybe $ byr p)) 
                        && (maybeBetweenInclusive 2010 2020 (readIntMaybe $ iyr p))
                        && (maybeBetweenInclusive 2020 2030 (readIntMaybe $ eyr p))
                        && (validateHeight $ hgt p)
                        && (validateHcl $ hcl p)
                        && (validateEcl $ ecl p)
                        && (validatePid $ pid p)

notIsNothing = (/=) Nothing
partOneValidation :: Passport -> Bool
partOneValidation p = (notIsNothing $ byr p) 
                      && (notIsNothing $ iyr p)
                      && (notIsNothing $ eyr p)
                        && (notIsNothing $ hgt p)
                        && (notIsNothing $ hcl p)
                        && (notIsNothing $ ecl p)
                        && (notIsNothing $ pid p)

main = do
        input <- readFile "input.txt"
        let part1 = numberOfLegalPassports partOneValidation input
        print $ "part 1:" ++ show part1
        let part2 = numberOfLegalPassports partTwoValidation input
        print $ "part 2:" ++ show part2
