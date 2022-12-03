module Solutions.Day3
  ( aoc3
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Text.Trifecta       (Parser)
import Text.Parser.Combinators
import Text.Parser.Token (token)
import Text.Parser.Char (letter)
import Data.Bifunctor (Bifunctor(bimap))
import Data.List (elemIndex, intersect)
import Data.Maybe (catMaybes, mapMaybe)
import Common.Debugging (traceLns)

aoc3 :: IO ()
aoc3 = do
  printSolutions 3 $ MkAoCSolution parseInput part1
  printSolutions 3 $ MkAoCSolution parseInput part2

parseInput :: Parser [String]
parseInput = do
  some $ token $ some letter

part1 :: [String] -> Int
part1 = sum . mapMaybe calculateScore . mapMaybe findDuplicate

findDuplicate:: String -> Maybe Char
findDuplicate s
  | null common = Nothing
  | otherwise = Just $ head common
  where (first, second) = splitString s
        common = first `intersect` second

calculateScore:: Char -> Maybe Int
calculateScore c = do
  i <- elemIndex c (['a'..'z'] ++ ['A'..'Z'])
  pure $ i + 1

splitString:: String -> (String, String)
splitString [] = ("", "")
splitString [x] = ([x], "")
splitString (x:x1) = bimap (x :) (last x1 :) rest
    where rest = splitString $ init x1

part2 :: [String] -> Int
part2 = sum . mapMaybe calculateScore . findItemTypes

findItemTypes:: [String] -> [Char]
findItemTypes s 
  | length s <= 3 = [head $ findBadge s]
  | otherwise = head (findBadge (take 3 s)) : findItemTypes (drop 3 s)
  where findBadge x = foldr intersect (head x) x
