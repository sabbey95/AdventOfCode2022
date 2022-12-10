module Solutions.Day10
    ( aoc10
    ) where

import           Common.AoCSolutions     (AoCSolution (MkAoCSolution),
                                          printSolutions, printTestSolutions)
import           Control.Applicative
import           Text.Parser.Char        (string)
import           Text.Parser.Combinators (some, try)
import           Text.Parser.Token       (token)
import           Text.Trifecta           (Parser, integer, newline)
import Common.Debugging (traceLns)
import Control.Monad (join)
import Data.List.Split (chunksOf)

aoc10 :: IO ()
aoc10 = do
  printSolutions 10 $ MkAoCSolution parseInput part1
  printSolutions 10 $ MkAoCSolution parseInput part2

type NoopCommand = String
type AddXCommand = Integer
data Command
  = AddXCommand Integer
  | NoopCommand String
  deriving (Show)

parseInput :: Parser [Command]
parseInput = do
  some $ token (try (AddXCommand <$> parseAddX) <|> try (NoopCommand <$> parseNoop))
  where parseNoop:: Parser NoopCommand
        parseNoop = do
          string "noop"
        parseAddX:: Parser AddXCommand
        parseAddX =  do
          string "addx "
          integer

part1 :: [Command] -> Int
part1 = sum . toSignalStrengths [20, 60,100,140,180,220] . calculateRegister

toSignalStrengths :: [Int] -> [Integer] -> [Int]
toSignalStrengths requiredCylces allCycles = map (\c -> c * fromIntegral (allCycles!!c)) requiredCylces

calculateRegister:: [Command] -> [Integer]
calculateRegister = foldl performCommand [1]

performCommand :: [Integer] -> Command -> [Integer]
performCommand cycles (NoopCommand _) = cycles ++ [last cycles]
performCommand cycles (AddXCommand x) = cycles ++ [lastCycle, lastCycle + x]
  where lastCycle = last cycles


part2 :: [Command] -> String
part2 = unlines . findLitPixels . calculateRegister

findLitPixels:: [Integer] -> [String]
findLitPixels register = map (zipWith (curry isLit) [0..]) $ chunksOf 40 register
  where isLit :: (Int, Integer) -> Char
        isLit (pos, reg) = if abs(toInteger pos - reg) <=1 then '#' else '.'
