module Solutions.Day4
    ( aoc4
    ) where

import           Common.AoCSolutions     (AoCSolution (MkAoCSolution),
                                          printSolutions, printTestSolutions)
import           Data.List               (maximumBy, minimumBy)
import           Data.Ord                (comparing)
import           Text.Parser.Combinators (some)
import           Text.Parser.Token       (token)
import           Text.Trifecta           (CharParsing (char), Parser, integer)

aoc4 :: IO ()
aoc4 = do
  printSolutions 4 $ MkAoCSolution parseInput part1
  printSolutions 4 $ MkAoCSolution parseInput part2

type Range = (Integer, Integer)
type ElfPair = (Range, Range)

parseInput :: Parser [ElfPair]
parseInput = do
  some $ token parseElfPair

parseElfPair:: Parser ElfPair
parseElfPair = do
  r1 <- parseRange
  char ','
  r2 <- parseRange
  pure $ sortRanges (r1, r2)
  where sortRanges (r1, r2) = if fst r1 <= fst r2 then (r1, r2) else (r2, r1)

parseRange :: Parser Range
parseRange = do
  fst <- integer
  char '-'
  snd <- integer
  pure (min fst snd, max fst snd)

part1 :: [ElfPair] -> Int
part1 = length . filter doRangesFullyOverlap

doRangesFullyOverlap:: ElfPair -> Bool
doRangesFullyOverlap ((min1, max1), (min2, max2)) 
  = max1 >= max2 || min1 == min2


part2 :: [ElfPair] -> Int
part2 = length . filter doRangesOverlapAtAll

doRangesOverlapAtAll:: ElfPair -> Bool
doRangesOverlapAtAll ((min1, max1), (min2, max2))
  = max1 >= min2 || doRangesFullyOverlap ((min1, max1), (min2, max2))
