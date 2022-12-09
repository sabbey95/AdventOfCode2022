module Solutions.Day9
    ( aoc9
    ) where

import           Common.AoCSolutions     (AoCSolution (MkAoCSolution),
                                          printSolutions, printTestSolutions)
import           Common.Debugging        (traceLns)
import qualified Control.Arrow           as Data.Bifunctor
import           Control.Monad.RWS       (MonadState (state))
import           Data.Bits               (Bits (xor))
import qualified Data.List
import qualified Data.Set                as S
import           Text.Parser.Combinators (some)
import           Text.Regex.TDFA.Common  (Position)
import           Text.Trifecta           (Parser, integer, letter, whiteSpace)

aoc9 :: IO ()
aoc9 = do
  printSolutions 9 $ MkAoCSolution parseInput part1
  printSolutions 9 $ MkAoCSolution parseInput part2


data Direction = UP | DOWN | LEFT | RIGHT deriving (Enum, Eq, Show)

data Instruction
  = Instruction
      { direction :: Direction
      , amount    :: Integer
      }
  deriving (Show)

type Point = (Int, Int)

data State
  = State
      { ropePosition      :: [Point]
      , visitedTailPoints :: S.Set Point
      }
  deriving (Show)

parseInput :: Parser [Instruction]
parseInput = do
  some parseInstruction
  where parseInstruction:: Parser Instruction
        parseInstruction = do
          char <- letter
          whiteSpace
          direction <- parseDirection char
          Instruction direction <$> integer
          where
            parseDirection d =
              case d of
                'R' -> pure RIGHT
                'U' -> pure UP
                'L' -> pure LEFT
                'D' -> pure DOWN
                _   -> fail "Unrecognised character"

initialState :: Int -> State
initialState length = State [(0,0) | _ <- [1..length]] S.empty

part1 :: [Instruction] -> Int
part1 = solve 2

part2 :: [Instruction] -> Int
part2 = solve 10

solve:: Int -> [Instruction] -> Int
solve trainSize instructions = S.size visitedPoints
  where State _ visitedPoints = foldl (runInstruction trainSize) (initialState trainSize) instructions

runInstruction :: Int -> State -> Instruction -> State
runInstruction trainSize state (Instruction _ 0) = state
runInstruction trainSize state (Instruction direction amount) = runInstruction trainSize newState (Instruction direction (amount - 1))
  where newState = makeMove trainSize direction state

makeMove :: Int -> Direction -> State -> State
makeMove trainSize d (State rope visitedTailPoints) = State resolvedTail (S.insert newTail visitedTailPoints)
  where newHead = moveHead d (head rope)
        updatedRope = replaceAtIndex 0 newHead rope
        resolvedTail = resolveTail (trainSize - 1) updatedRope
        newTail = last resolvedTail

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex i x xs = take i xs ++ [x] ++ drop (i+1) xs

moveHead:: Direction -> Point -> Point
moveHead d (x, y) = case d of
    UP    -> (x, y + 1)
    DOWN  -> (x, y - 1)
    LEFT  -> (x - 1, y)
    RIGHT -> (x + 1, y)

resolveTail:: Int -> [Point] -> [Point]
resolveTail 0 points = points
resolveTail x points  = resolveTail (x-1) (replaceAtIndex index newCurrent points)
  where index = length points - x
        previous = points!!(index-1)
        current = points!!index
        newCurrent = moveCurrent previous current

moveCurrent :: Point -> Point -> Point
moveCurrent (x1, y1) (x2, y2) = if requiresMove then (x1 - dx, y1 - dy) else (x2, y2)
  where dx = magnitudeOfMove x1 x2
        dy = magnitudeOfMove y1 y2
        requiresMove = dx /=0 || dy/=0

magnitudeOfMove:: Int -> Int -> Int
magnitudeOfMove n1 n2
  | n1 - n2 > 1 = 1
  | n1 - n2 < -1 = -1
  | otherwise = 0    