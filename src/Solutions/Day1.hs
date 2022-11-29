module Solutions.Day1
  ( aoc1
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Data.List           (tails)
import           Text.Trifecta       (Parser, TokenParsing (token), integer,
                                      some)
import Common.ListUtils (window3, window2)

aoc1 :: IO ()
aoc1 = do
  printSolutions 1 $ MkAoCSolution parseInput part1
  printSolutions 1 $ MkAoCSolution parseInput part2

type Depths = [Integer]

parseInput :: Parser Depths
parseInput = undefined

part1 :: Depths -> Int
part1 = undefined

part2 :: Depths -> Int
part2 = undefined

