module Solutions.Day1
  ( aoc1
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import Data.List ( tails, sort )
import Text.Trifecta
    ( Parser,
      TokenParsing(token),
      integer,
      some,
      Parsing(unexpected),
      sepBy,
      CharParsing(string),
      optional,
      oneOf,
      characterChar,
      whiteSpace,
      integer' )
import Common.ListUtils (window3, window2)
import Text.Parser.Char (newline)
import Text.Parser.Combinators
import Text.Read (Lexeme(String))
import Combinatorics.Mastermind (Eval(white))

aoc1 :: IO ()
aoc1 = do
  printSolutions 1 $ MkAoCSolution parseInput part1
  printSolutions 1 $ MkAoCSolution parseInput part2

type Inventory = [Integer]

parseInput :: Parser [Inventory]
parseInput = do
  some parseInventory

parseInventory :: Parser Inventory
parseInventory = do
  inv <- some parseInt
  optional newline
  pure inv

parseInt :: Parser Integer
parseInt = do
  i <- integer'
  newline
  pure i

part1 :: [Inventory] -> Integer
part1 = solve 1

part2 :: [Inventory] -> Integer
part2 = solve 3

solve :: Int -> [Inventory] -> Integer
solve n = sum .take n . reverse . sort . map sum