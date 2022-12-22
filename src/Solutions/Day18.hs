module Solutions.Day18
    ( aoc18
    ) where

import           Common.AoCSolutions     (AoCSolution (MkAoCSolution),
                                          printSolutions, printTestSolutions)
import           Common.Debugging        (traceLns)
import           Data.List               (group, intersect, nub, partition,
                                          sort)
import           Data.Maybe              (isNothing)
import qualified Data.Set                as S
import           Text.Parser.Combinators (some)
import           Text.Trifecta           (Parser, TokenParsing (token), comma,
                                          integer)

aoc18 :: IO ()
aoc18 = do
  printSolutions 18 $ MkAoCSolution parseInput part1
  printSolutions 18 $ MkAoCSolution parseInput part2

type Cube = (Integer, Integer, Integer)

parseInput :: Parser [Cube]
parseInput = do
  some $ token parseCube
  where
    parseCube = do
      x <- integer
      comma
      y <- integer
      comma
      z <- integer
      pure (x, y, z)

part1 :: [Cube] -> Int
part1 cubes =  length . filter (`notElem` cubes) $ concatMap findAdjacent cubes

offsets = [(1,0,0), (-1,0,0), (0,1,0), (0,-1,0), (0,0,1), (0,0,-1)]


part2 :: [Cube] -> Int
part2 cubes = part1 (cubes ++ closedEdges)
  where maxX = maximum (map (\(x, _, _) -> x) cubes)
        maxY = maximum (map (\(_, y, _) -> y) cubes)
        maxZ = maximum (map (\(_, _, z) -> z) cubes)
        emptyPoints = [(x, y, z) | x <- [-1..maxX+1], y <- [-1..maxY+1], z <- [-1..maxZ+1], (x, y, z) `notElem` cubes]
        groupedPoints = clusterPoints $ map S.singleton emptyPoints
        closedEdges = concat $ filter (\c -> (-1, -1, -1) `notElem` c) (map S.toList groupedPoints)

clusterPoints :: [S.Set Cube] -> [S.Set Cube]
clusterPoints = foldr combineClusters []

combineClusters :: S.Set Cube -> [S.Set Cube] -> [S.Set Cube]
combineClusters cubes [] = [cubes]
combineClusters cubes clusters = S.unions (cubes : matching) : nonMatching
      where (matching, nonMatching) = partition (touchingClusters cubes) clusters

touchingClusters :: S.Set Cube -> S.Set Cube -> Bool
touchingClusters cluster1 cluster2 = not . S.null . S.intersection cluster2 $ S.fromList (concatMap findAdjacent $ S.toList cluster1)

findAdjacent:: Cube -> [Cube]
findAdjacent (x, y, z) = map (\(dx, dy, dz) -> (x + dx, y + dy, z + dz)) offsets
