module Solutions.Day23
    ( aoc23
    ) where

import           Common.AoCSolutions     (AoCSolution (MkAoCSolution),
                                          printSolutions, printTestSolutions)
import           Common.Debugging        (traceLns)
import           Data.Bifunctor          (Bifunctor (bimap))
import           Data.List               (nub)
import qualified Data.Map                as M
import           Text.Parser.Char        (oneOf)
import           Text.Parser.Combinators (some)
import           Text.Parser.Token       (token)
import           Text.Trifecta           (CharParsing (anyChar), Parser)

aoc23 :: IO ()
aoc23 = do
  printSolutions 23 $ MkAoCSolution parseInput part1
  printTestSolutions 23 $ MkAoCSolution parseInput part2

type Point = (Integer, Integer)
data Direction = N | E | S | W deriving (Enum, Eq, Ord, Show)

parseInput :: Parser [Point]
parseInput = do
  lines <- some $ token $ some $ oneOf ['.', '#']
  pure $ linesToPoints lines

linesToPoints:: [String] -> [Point]
linesToPoints lines =  map fst . filter snd $ concatMap toPoints withRows
  where withColumns = map (zip [0..]) lines
        withRows = zip [0..] withColumns
        toPoints (row, columns) = map (\(column, char) -> ((column, row), char == '#')) columns

part1 :: [Point] -> Integer
part1 = countEmptySpaces . doMoves 10 (cycle [N,S,W, E])

doMoves :: Integer -> [Direction] -> [Point] -> [Point]
doMoves 0 _ elves = elves
doMoves round directions elves = doMoves (round -1) (drop 1 directions) newElves
  where possibleMovements = map (findPossibleMovements elves (take 4 directions)) elves
        allDestinations = map snd $ filter (uncurry (/=)) possibleMovements
        newElves = map (\(old, new) -> if length (filter (new==) allDestinations) > 1 then old else new) possibleMovements


findPossibleMovements :: [Point] -> [Direction] -> Point -> (Point, Point)
findPossibleMovements allElves directions elf
    | elfNotInSpaces allElves (concat $ M.elems directionsToCheck) elf  = (elf, elf)
    | otherwise = tryDirections allElves directions elf

tryDirections :: [Point] -> [Direction] -> Point -> (Point, Point)
tryDirections _ [] elf = (elf, elf)
tryDirections allElves (direction:others) elf
  | elfNotInSpaces allElves (M.findWithDefault [] direction directionsToCheck) elf = (elf, moveElf elf direction)
  | otherwise = tryDirections allElves others elf

moveElf :: Point -> Direction -> Point
moveElf (x, y) d = case d of
  N -> (x, y - 1)
  E -> (x+1, y)
  S -> (x, y+1)
  W -> (x-1, y)

elfNotInSpaces:: [Point] -> [Point] -> Point -> Bool
elfNotInSpaces allElves pointsToCheck elf = not (any (`elem` allElves) allAdjacentSpaces)
  where allAdjacentSpaces = map (bimap (fst elf +) (snd elf +)) pointsToCheck

directionsToCheck = M.fromList [
  (N, [(-1,-1), (0, -1), (1, -1)]),
  (E, [(1,0), (1, 1), (1, -1)]),
  (S, [(-1,1), (0, 1), (1, 1)]),
  (W, [(-1,1), (-1, 0), (-1, -1)])
  ]

countEmptySpaces :: [Point] -> Integer
countEmptySpaces elves = ((width+1) * (height+1)) - toInteger (length elves)
  where width = maximum (map fst elves) - minimum (map fst elves)
        height = maximum (map snd elves) - minimum (map snd elves)

part2 :: [Point] -> Integer
part2 = firstNonMover (cycle [N,S,W, E])

firstNonMover :: [Direction] -> [Point] -> Integer
firstNonMover directions elves
 | not $ any (uncurry (/=)) possibleMovements = 1
 | otherwise = 1 + firstNonMover  (drop 1 directions) newElves
  where possibleMovements = map (findPossibleMovements elves (take 4 directions)) elves
        allDestinations = map snd $ filter (uncurry (/=)) possibleMovements
        newElves = map (\(old, new) -> if length (filter (new==) allDestinations) > 1 then old else new) possibleMovements