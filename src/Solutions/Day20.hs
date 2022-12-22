module Solutions.Day20
  ( aoc20
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Text.Trifecta       (Parser, integer, TokenParsing (token), DeltaParsing (position))
import Text.Parser.Combinators (some)
import Data.Foldable (find)
import Data.Maybe (fromMaybe)
import Data.List (sortBy, sortOn)
import Data.Ord (comparing)
import Data.Function (on)
import Common.Debugging (traceLns)
import Linear (normalize)

aoc20 :: IO ()
aoc20 = do
  printSolutions 20 $ MkAoCSolution parseInput part1
  printSolutions 20 $ MkAoCSolution parseInput part2

parseInput :: Parser [Integer]
parseInput = do
  some $ token integer

part1 :: [Integer] -> Integer
part1 x = solve resolved
  where positions = toPositions x
        resolved = nextMovement positions positions


nextMovement :: [(Integer, Integer, Integer)] -> [(Integer, Integer, Integer)] -> [(Integer, Integer, Integer)]
nextMovement [] currentPositions = currentPositions
nextMovement (next:others) currentPositions = nextMovement others afterMove
    where afterMove = makeMove currentPositions next

makeMove :: [(Integer, Integer, Integer)] -> (Integer, Integer, Integer) -> [(Integer, Integer, Integer)]
makeMove currentPositions (value, key, _) = fromMaybe currentPositions newPositions
  where l = toInteger $ length currentPositions
        newPositions = do
          (_, _ , currentPosition) <- find (\(_, k, _) -> k == key) currentPositions
          let movedPosition =sortOutValue (currentPosition + value) l
          let moveMag = if movedPosition > currentPosition then -1 else 1
          Just $ map (\(v, k, p ) -> if k == key then (v, k, movedPosition) else if (p <= movedPosition && p >= currentPosition) || (p >= movedPosition && p <= currentPosition) then (v, k, p + moveMag) else (v, k, p)) currentPositions

sortOutValue:: Integer -> Integer -> Integer
sortOutValue i l 
  | i >= l = sortOutValue ((i `mod` l) + (i `div` l)) l
  | i < 0 = sortOutValue ((i `mod` l) + (i `div` l)) l
  | otherwise = i

toPositions:: [Integer] -> [(Integer, Integer, Integer)]
toPositions = zipWith (\p i -> (i, p , p)) [0..]

part2 :: [Integer] -> Integer
part2 x = solve (resolveMoves positions 10)
  where positions = toPositions (normalise x)

resolveMoves:: [(Integer, Integer, Integer)] -> Integer -> [(Integer, Integer, Integer)]  
resolveMoves positions 1 = nextMovement positions positions
resolveMoves positions t = nextMovement positions resolution 
  where resolution = resolveMoves positions (t - 1)


solve :: [(Integer, Integer, Integer)] -> Integer
solve resolved = sum . map (\(v, _,_) -> v) $ filter (\(v, k, p) -> p `elem` caredForIs) resolved
  where l = toInteger $ length resolved
        (_, _, zeroP) = head $ filter (\(v, _, _) -> v == 0) resolved
        caredForIs = map (\c -> (c + zeroP) `mod` l) [1000,2000,3000]        


normalise:: [Integer] -> [Integer]
normalise = map (*811589153)