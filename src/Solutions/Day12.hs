module Solutions.Day12
  ( aoc12
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Text.Trifecta       (Parser, letter)
import qualified Data.Map as M
import Text.Parser.Combinators (some)
import Text.Parser.Token (token)
import Data.Foldable (find)
import qualified Control.Lens as Data.Bifunctor
import Data.Maybe (mapMaybe, fromMaybe)
import Common.Debugging (traceLns)
import Debug.Trace (trace)
import Data.List.Split (chunksOf)

aoc12 :: IO ()
aoc12 = do
  printSolutions 12 $ MkAoCSolution parseInput part1
  printSolutions 12 $ MkAoCSolution parseInput part2

type Point = (Int, Int)
type HeightMap = M.Map Point Char
type WeightMap = M.Map Point Int

parseInput :: Parser HeightMap
parseInput = do
  lines <- some $ token $ some letter
  pure $ mapLines lines

mapLines :: [[Char]] -> HeightMap
mapLines lines = foldr mapLine M.empty numberedLines
    where numberedLines = zip lines [0..]

mapLine :: ([Char], Int) -> HeightMap -> HeightMap
mapLine (line, row) currentHeights = foldr insertPoint currentHeights numberedChars
    where numberedChars = zip line [0..]
          insertPoint (char, column) = M.insert (column, row) char

part1 :: HeightMap -> Maybe Int
part1 x = M.lookup start $ solve x
  where start = head $ lookupPoint 'S' x

solve ::  HeightMap -> WeightMap
solve x =  findShortestPath x start end $ M.fromList [(end, 0)]
  where start = head $ lookupPoint 'S' x
        end = head $ lookupPoint 'E' x


findShortestPath :: HeightMap -> Point -> Point -> WeightMap -> WeightMap
findShortestPath heightMap destination current weights
  | destination == current = weights
  | null realLowerPoints = weights
  | otherwise = foldr (findShortestPath heightMap destination) updatedWeightMap realLowerPoints
    where adjacentPoints = map (Data.Bifunctor.bimap (fst current +) (snd current +))[(1,0), (0,1), (-1, 0), (0, -1)]
          findHeight p = do
            height <- M.lookup p heightMap
            Just (height, p)
          currentWeight = M.findWithDefault 0 current weights
          lowerPoints = do
            currentHeight <- M.lookup current heightMap
            adjacentHeights <- Just (mapMaybe findHeight adjacentPoints)
            Just . map snd . filter (isTraversable currentHeight) $ adjacentHeights
          realLowerPoints = filter (fromMaybe True . isWeightReduction currentWeight weights) $ fromMaybe [] lowerPoints
          updatedWeightMap = addWeights currentWeight weights realLowerPoints

isWeightReduction :: Int -> WeightMap -> Point -> Maybe Bool
isWeightReduction weight weights p = do
    pointWeight <- M.lookup p weights
    Just $ pointWeight > weight + 1


addWeights :: Int -> WeightMap -> [Point] -> WeightMap
addWeights current = foldr (update current)

update :: Int -> Point -> WeightMap -> WeightMap
update i p = M.insertWith min p (i + 1)

isTraversable :: Char -> (Char, Point) -> Bool
isTraversable destination (current, _) = fromMaybe False result
  where charToHeight = M.fromList (zip ['a'..'z'] [1..] ++ [('S', 1), ('E', 26)])
        result = do
          destHeight <- M.lookup destination charToHeight
          currentHeight <- M.lookup current charToHeight
          Just $ destHeight <= (currentHeight + 1)

lookupPoint :: Char -> HeightMap ->  [Point]
lookupPoint c m =  map fst . filter (\(_, char) -> char == c) $ M.assocs m

part2 :: HeightMap -> Int
part2 x = minimum $ mapMaybe (`M.lookup` weights) starts
  where starts = lookupPoint 'a' x
        weights = solve x