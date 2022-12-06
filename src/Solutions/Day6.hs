module Solutions.Day6
    ( aoc6
    ) where

import           Common.AoCSolutions     (AoCSolution (MkAoCSolution),
                                          printSolutions)
import qualified Data.Set                as S
import           Text.Parser.Combinators (some)
import           Text.Trifecta           (CharParsing (anyChar), Parser)

aoc6 :: IO ()
aoc6 = do
  printSolutions 6 $ MkAoCSolution parseInput part1
  printSolutions 6 $ MkAoCSolution parseInput part2

parseInput :: Parser String
parseInput = do
  some anyChar

part1 :: String -> Int
part1 = solve 4

solve::  Int -> String -> Int
solve = checkPacket 0

part2 :: String -> Int
part2 = solve 14

checkPacket:: Int -> Int -> String -> Int
checkPacket index messageLength str
  | length str < messageLength = index
  | otherwise =  if S.size (S.fromList (take messageLength str)) == messageLength 
                  then index + messageLength 
                  else checkPacket (index+1) messageLength (tail str)
