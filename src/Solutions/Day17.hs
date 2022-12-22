module Solutions.Day17
    ( aoc17
    ) where

import           Common.AoCSolutions     (AoCSolution (MkAoCSolution),
                                          printSolutions, printTestSolutions)
import           Text.Parser.Combinators (some)
import           Text.Trifecta           (CharParsing (anyChar), Parser,
                                          TokenParsing (token))
import Common.Debugging (traceLns)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

aoc17 :: IO ()
aoc17 = do
  printSolutions 17 $ MkAoCSolution parseInput part1
  printSolutions 17 $ MkAoCSolution parseInput part2

data Command = L | R | D deriving (Enum, Eq, Show)

parseInput :: Parser [Command]
parseInput = do
  some $ token parseCommand
  where parseCommand = do
          c <- anyChar
          case c of
            '>' -> pure R
            '<' -> pure L
            _   -> fail "Noope"


type Point = (Integer, Integer)
type Rock = [Point]

type State = (Integer, Integer, [Point])
type StateMap = M.Map State (Integer, Integer)

rocks:: [Rock]
rocks = [
  [(2,3), (3,3), (4,3), (5,3)],
  [(3,3), (2,4), (3, 4), (4, 4), (3, 5)],
  [(2,3), (3, 3), (4, 3), (4, 4), (4, 5)],
  [(2,3), (2, 4), (2, 5), (2, 6)],
  [(2,3), (2,4), (3, 3), (3, 4)]
  ]

part1 :: [Command] -> Integer
part1 =  solve 2022

part2 :: [Command] -> Integer
part2  = solve 1000000000000

intersectsStoppedRock :: Point -> [Rock] -> Bool
intersectsStoppedRock p = any (elem p)

tryCommand :: Command -> Rock -> Rock
tryCommand command rock =
  case command of
    L -> moveRock (-1) 0
    R -> moveRock 1 0
    D -> moveRock 0 (-1)
  where moveRock dx dy = map (\(x, y) -> (x + dx, y + dy)) rock


solve:: Integer -> [Command] -> Integer
solve cycles commands = runCommandsUntilRepeat cycles (cycle commands) (drop 1 infiniteRocks) M.empty (initialRockPosition [] $ head rocks) [] (0, toInteger $ length commands) (0, toInteger $ length rocks)
    where infiniteRocks = cycle rocks

runCommandsUntilRepeat :: Integer -> [Command] -> [Rock] -> StateMap -> Rock -> [Point] -> (Integer, Integer) ->  (Integer, Integer) -> Integer
runCommandsUntilRepeat cycles commands rocks stateMap movingRock stoppedRocks (commandsDone, lCommands) (rocksDone, lRocks)
 | rocksDone >= cycles = maxHeight stoppedRocks
 | newRock = runCommandsUntilRepeat cycles (drop 1 commands) (drop 1 rocks) newStateMap (initialRockPosition reallyNewStoppedRocks $ head rocks) reallyNewStoppedRocks (commandsDone + 1, lCommands) (newRocksDone, lRocks)
 | otherwise = runCommandsUntilRepeat cycles (drop 1 commands) rocks stateMap fallenRock stoppedRocks (commandsDone + 1, lCommands) (rocksDone, lRocks)
 where
  mHeight = maxHeight stoppedRocks
  validRockPlacement r = not $ any (\(x, y) -> x < 0 || x >= 7 || y < 0 || y < (mHeight - heightOffset) || (x,y) `elem` stoppedRocks) r
  movedRock = tryCommand (head commands) movingRock
  newRockPlace = if validRockPlacement movedRock then movedRock else movingRock
  fallenRock = tryCommand D newRockPlace
  newStoppedRocks = filter (\(_, y) -> y > mHeight - heightOffset) $ stoppedRocks ++ newRockPlace

  newRock = not (validRockPlacement fallenRock)
  newMHeight = if newRock then maxHeight newStoppedRocks else mHeight

  newState = (commandsDone `mod` lCommands, rocksDone `mod` lRocks, map (\(x, y) -> (x, y - newMHeight)) newStoppedRocks)
  increments = do
      (oldHeight, oldRocks) <- if newRock then M.lookup newState stateMap else Nothing
      let heightDif = newMHeight - oldHeight
      let cycleLength = 1 + rocksDone - oldRocks
      let doneCycles = (cycles - rocksDone) `div` cycleLength
      pure (cycleLength * doneCycles, heightDif * doneCycles)

  (normRockIncrement, normHeightIncrement) = fromMaybe (0, 0) increments

  reallyNewStoppedRocks = map (\(x, y) -> (x, y + normHeightIncrement)) newStoppedRocks
  newRocksDone = rocksDone + normRockIncrement + 1

  newStateMap = if newRock && mHeight > heightOffset then M.insert newState (newMHeight + normHeightIncrement, newRocksDone) stateMap else stateMap


initialRockPosition :: [Point] -> Rock -> Rock
initialRockPosition currentRocks = map (\(x, y) -> (x, y + floor))
  where floor = maxHeight currentRocks


maxHeight :: [Point] -> Integer
maxHeight []    = 0
maxHeight rocks = 1 + maximum (map snd rocks)


heightOffset = 50