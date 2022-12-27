module Solutions.Day24
    ( aoc24
    ) where

import           Common.AoCSolutions     (AoCSolution (MkAoCSolution),
                                          printSolutions, printTestSolutions)
import           Common.Debugging        (traceLns)
import qualified Data.Map                as M
import           Data.Maybe              (fromMaybe, isJust, isNothing,
                                          mapMaybe)
import qualified Data.Set                as S
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Parser.Token
import           Text.Trifecta           (Parser)

aoc24 :: IO ()
aoc24 = do
  -- printTestSolutions 24 $ MkAoCSolution parseInput part1
  printSolutions 24 $ MkAoCSolution parseInput part2


type Point = (Integer, Integer)
data Direction = N | E | S | W deriving (Enum, Eq, Ord, Show)
type Blizzards =  [(Point, Direction)]
data State
  = State
      { blizzards :: Blizzards
      , current   :: Point
      , end       :: Point
      , limit ::     Point
      }
  deriving (Eq, Ord, Show)

parseInput :: Parser State
parseInput = do
  lines <- some $ token $ some $ notChar '\n'
  pure $ linesToInput lines

linesToInput :: [String] -> State
linesToInput lines = State (linesToBlizzards lines) (1, 0) (width - 2, height - 1) (width - 2, height - 1)
  where width = toInteger $ length (head lines)
        height = toInteger $ length lines

linesToBlizzards:: [String] -> Blizzards
linesToBlizzards lines = concatMap toPoints withRows
  where withColumns = map (zip [0..]) lines
        withRows = zip [0..] withColumns

toPoints :: (Integer, [(Integer, Char)]) -> [(Point, Direction)]
toPoints (row, columns) = mapMaybe toPoint columns
  where toPoint:: (Integer, Char) -> Maybe (Point, Direction)
        toPoint (column, char)
          = case char of
            'v' -> pure ((column, row), S)
            '>' -> pure ((column, row), E)
            '^' -> pure ((column, row), N)
            '<' -> pure ((column, row), W)
            _   -> Nothing


part1 :: State  -> Integer
part1 = solve

solve :: State  -> Integer
solve s = minimum . M.elems . M.filterWithKey (\k _ -> fst k == end s)  $ doMovement M.empty (allBlizzards s hackyHighNumber) 0 s

allBlizzards:: State -> Integer -> M.Map Integer Blizzards
allBlizzards (State blizzards current end limit) top = fst $ foldl (\(m, b) i -> (M.insert i (moveBlizzards (State b current end limit)) m, moveBlizzards (State b current end limit))) (M.fromList [(-1, blizzards)], blizzards) [0..top]

hackyHighNumber = 400
modder = lcm 120 25

type Value = (Point, Integer)

doMovement :: M.Map Value Integer -> M.Map Integer Blizzards -> Integer -> State ->  M.Map Value Integer
doMovement currentMinimums pastBlizzards currentSteps (State currentBlizzards current end limit)
  | current == end = M.insert value currentSteps currentMinimums
  | current `elem` map fst currentBlizzards = currentMinimums
  | M.findWithDefault hackyHighNumber value currentMinimums <= currentSteps = currentMinimums
  | otherwise = foldr (next pastBlizzards (State afterBlizzard current end limit) currentSteps) newMap potentialMoves
  where afterBlizzard = fromMaybe [] (M.lookup currentSteps pastBlizzards)
        potentialMoves = getPotentialMoves (State afterBlizzard current end limit)
        value = (current, currentSteps `mod` modder)
        newMap = M.insertWith min value currentSteps currentMinimums

manhattanDistance:: Point -> Point -> Integer
manhattanDistance (x1, y1) (x2,y2) = abs (x1 -x2) + abs (y1 - y2)

next :: M.Map Integer Blizzards -> State  -> Integer -> Point ->  M.Map Value Integer ->  M.Map Value Integer
next past (State blizzards _ end limit) steps p currentMin  = doMovement currentMin past (steps + 1) (State blizzards p end limit)

getPotentialMoves :: State -> [Point]
getPotentialMoves (State blizzards current end limit) = filter (`notElem` map fst blizzards) (mapMaybe  (movePerson current end limit) [E,S,W, N]) ++ [current]

movePerson :: Point -> Point ->  Point -> Direction -> Maybe Point
movePerson (x, y) (endX, endY) (limitX, limitY) direction = case direction of
          N -> if y <= 1 && (x /= endX || x >= 3) then Nothing else Just (x, y - 1)
          E -> if x >= limitX || y == 0  || y==limitY  then Nothing else Just (x + 1, y)
          S -> if y >= limitY - 1 && (x /= endX || x < 4) then Nothing else Just (x, y + 1)
          W -> if x <= 1 || y == 0 || y==limitY  then Nothing else Just (x - 1, y)


moveBlizzards :: State -> Blizzards
moveBlizzards (State blizzards _ _ limit) = newBlizzards
  where newBlizzards = map (moveBlizzard limit) blizzards

moveBlizzard :: Point -> (Point, Direction) -> (Point, Direction)
moveBlizzard (endX, endY) ((x, y), direction) = (point, direction)
  where
    point = case direction of
      N -> (x, if y <= 1 then endY - 1 else y - 1)
      E -> (if x >= endX then 1 else x + 1, y)
      S -> (x, if y >= endY - 1 then 1 else y + 1)
      W -> (if x <= 1 then endX else x - 1, y)


part2 :: State -> Integer
part2 (State blizzards current end limit) = there + back + solve (State backBlizzard current end limit)
  where there = solve (State blizzards current end limit)
        thereBlizzard = do
          traceLns ("There " ++ show there)
           M.findWithDefault [] (there - 1) $ allBlizzards (State blizzards current end limit) there
        back = solve (State thereBlizzard end current limit)
        backBlizzard = do
          traceLns ("Back " ++ show back)
           M.findWithDefault [] (back - 1) $ allBlizzards (State thereBlizzard current end limit) back
