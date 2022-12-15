module Solutions.Day14
  ( aoc14
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Text.Trifecta       (Parser, sepBy, integer, comma, Parsing (try), optional, sepBy1, lower, parens)
import Text.Parser.Token (token)
import Text.Parser.Combinators ( some, eof )
import Text.Parser.Char (string)
import qualified Data.Map as M
import Data.Maybe (isNothing)

aoc14 :: IO ()
aoc14 = do
  printSolutions 14 $ MkAoCSolution parseInput part1
  printSolutions 14 $ MkAoCSolution parseInput part2

type Point = (Integer, Integer)
type Line = [Point]

data Filling = Wall | Sand | Air deriving (Enum, Show, Eq)
type Cave = M.Map Point Filling

parseInput :: Parser [Line]
parseInput = do
  some $ token parseLine
  where parseLine:: Parser Line
        parseLine = do
          sepBy1 parsePoint $ string "-> "

parsePoint :: Parser Point
parsePoint = do
  x <- integer
  comma
  y <- integer
  pure (x, y)

part1 :: [Line] -> Integer
part1 = solve False 0

solve:: Bool -> Integer -> [Line] -> Integer
solve infiniteFloor floorDepth lines = dropSand infiniteFloor cave lowestWall
  where cave = buildCave lines
        lowestWall = floorDepth + (maximum . map snd $ concat lines)


dropSand :: Bool -> Cave -> Integer -> Integer
dropSand infiniteFloor cave lowestWall
  | sandLandingPoint == entryPoint = 1 + countSand
  | snd sandLandingPoint > lowestWall = countSand
  | otherwise = dropSand infiniteFloor (M.insert sandLandingPoint Sand cave) lowestWall
  where sandLandingPoint = findSandLandingPoint infiniteFloor cave lowestWall entryPoint
        countSand = toInteger . length . filter (==Sand) $ M.elems cave

findSandLandingPoint :: Bool -> Cave -> Integer -> Point -> Point
findSandLandingPoint infiniteFloor cave lowestWall (x, y)
  | y > lowestWall = (x, y)
  | infiniteFloor && y == lowestWall - 1 = (x, y)
  | checkPoint (x, y+1) = nextPoint (x, y+1)
  | checkPoint (x-1, y+1) = nextPoint (x-1, y+1)
  | checkPoint (x +1, y+1) = nextPoint (x+1, y+1)
  | otherwise = (x,y)
  where checkPoint = isEmptyPoint cave
        nextPoint = findSandLandingPoint infiniteFloor cave lowestWall

isEmptyPoint:: Cave -> Point -> Bool
isEmptyPoint cave p = M.findWithDefault Air p cave == Air

buildCave:: [Line] -> Cave
buildCave = foldr addLine M.empty

addLine :: Line -> M.Map Point Filling -> M.Map Point Filling
addLine (p1:p2:others) m = M.union ( M.fromList (getPoints p1 p2)) $ addLine (p2 : others) m
addLine _ m = m

getPoints :: Point -> Point -> [(Point, Filling)]
getPoints (x1, y1) (x2, y2) = [((x, y), Wall )| x <- [(min x1 x2)..(max x1 x2)], y <- [(min y1 y2)..(max y1 y2)]]

entryPoint = (500, 0) ::Point

part2 :: [Line] -> Integer
part2 = solve True 2
