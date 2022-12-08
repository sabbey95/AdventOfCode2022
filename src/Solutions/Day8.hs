{-# LANGUAGE TupleSections #-}
module Solutions.Day8
  ( aoc8
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Text.Trifecta       (Parser, octDigit, parens, colon)
import Text.Parser.Combinators (some)
import Text.Parser.Char (digit)
import Text.Parser.Token (token)
import Data.Set (Set)
import qualified Data.Set as S
import Common.ListUtils (window2)
import Data.List (findIndex, transpose, find)
import Data.Maybe (catMaybes, fromMaybe)
import Common.Debugging (traceLns)
import Debug.Trace (trace)

aoc8 :: IO ()
aoc8 = do
  printSolutions 8 $ MkAoCSolution parseInput part1
  printSolutions 8 $ MkAoCSolution parseInput part2

type Grid = [[Integer]]
type Point = (Int, Int)

parseInput :: Parser Grid
parseInput = do
  some $ token $ some parseDigit
  where parseDigit:: Parser Integer
        parseDigit = do
          dig <- digit
          pure $ read [dig]

part1 :: Grid -> Int
part1 grid = S.size allPoints
    where rowPoints = checkRows grid 0
          columnPoints = S.map (\c -> (snd c, fst c)) $ checkRows (transpose grid) 0
          allPoints = S.union rowPoints columnPoints

checkRows:: Grid -> Int -> S.Set Point
checkRows [] _ = S.empty
checkRows (row:others) index = S.union newPoints $ checkRows others (index + 1)
  where
        pointsForward = map (index,) $ treeWindows row
        pointsBackwards = map (\c -> (index, length row - c - 1)) $ treeWindows $ reverse row
        newPoints = S.fromList $ pointsForward ++ pointsBackwards

treeWindows:: [Integer] -> [Int]
treeWindows [] = []
treeWindows x = if isVisible then (length x - 1) : otherTreeWindows else otherTreeWindows
  where isVisible = all (\c -> c < last x) (init x)
        otherTreeWindows = treeWindows (init x)

part2 :: Grid -> Int
part2 grid  = maximum . map (findScenicDistance grid) $ allPoints grid

allPoints:: Grid -> [Point]
allPoints grid = [(x, y) | x <- [1..(length grid - 2)],  y <- [1..(length (head grid) - 2)]]

findScenicDistance:: Grid -> Point -> Int
findScenicDistance grid p = product [treesBackwards, treesDown, treesForward, treesUp]
  where row = grid!!fst p
        column = transpose grid!!snd p
        treesForward = firstBlocker (drop (snd p + 1) row) $ row!!snd p
        treesBackwards = firstBlocker (reverse $ take (snd p) row) $ row!!snd p
        treesDown = firstBlocker (drop (fst p + 1) column) $ column!! fst p
        treesUp = firstBlocker (reverse $ take (fst p) column) $ column!! fst p

firstBlocker:: [Integer] -> Integer -> Int
firstBlocker path height = fromMaybe (length path - 1) (findIndex (>=height) path) + 1