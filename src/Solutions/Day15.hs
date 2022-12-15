module Solutions.Day15
    ( aoc15
    ) where

import           Common.AoCSolutions     (AoCSolution (MkAoCSolution),
                                          printSolutions, printTestSolutions)
import           Common.Debugging        (traceLns)
import           Control.Lens            (Contravariant (contramap))
import           Data.Function           (on)
import           Data.List               (sort, sortBy, nub)
import           Data.Maybe              (mapMaybe)
import qualified Data.Set                as S
import           Debug.Trace             (trace)
import           Text.Parser.Char
import           Text.Parser.Combinators (some)
import           Text.Trifecta           (Parser, TokenParsing (token), integer,
                                          sepBy)

aoc15 :: IO ()
aoc15 = do
  printSolutions 15 $ MkAoCSolution parseInput part1
  printSolutions 15 $ MkAoCSolution parseInput part2

type Point = (Integer, Integer)
type XRange = (Integer, Integer)
type SensorReport = (Point, Point)


parseInput :: Parser [SensorReport]
parseInput = do
  some $ token parseSensorReport
  where
    parseSensorReport = do
      string "Sensor at x="
      sX <- integer
      string ", y="
      sY <- integer
      string ": closest beacon is at x="
      bX <- integer
      string ", y="
      bY <- integer
      pure ((sX, sY), (bX, bY))


part1 :: [SensorReport] -> Integer
part1 x = sum (map sumRange range) - toInteger (length $ nub (filter (\(x, y) -> y == row) beacons))
  where row = 2000000
        beacons = map snd x
        range = reduceRanges $ mapMaybe (findPointsRange row) x

findPointsRange :: Integer -> SensorReport -> Maybe (Integer, Integer)
findPointsRange row sensorReport
  | relativeDistance < 0 = Nothing
  | otherwise = Just (sX - relativeDistance, sX + relativeDistance)
    where beaconDistance = getBeaconDistance sensorReport
          (sX, sY) = fst sensorReport
          difference = abs (row - sY)
          relativeDistance = beaconDistance - difference

sumRange :: XRange -> Integer
sumRange (start, end) = 1 + end - start

getBeaconDistance:: SensorReport -> Integer
getBeaconDistance ((sX, sY),(bX, bY)) = abs (sX - bX) + abs(sY -bY)

part2 :: [SensorReport] -> Maybe Integer
part2 = tuningFrequency constraints constraints
  where constraints = 4000000

tuningFrequency :: Integer -> Integer -> [SensorReport] -> Maybe Integer
tuningFrequency row maxRow sensorReports
  | row < 0 = Nothing
  | length pointsRanges == 1 = tuningFrequency (row - 1) maxRow sensorReports
  | otherwise = Just (4000000 * (snd (head pointsRanges) + 1) + row)
  where pointsRanges = reduceRanges $ mapMaybe (findPointsRange row) sensorReports


reduceRanges:: [XRange] -> [XRange]
reduceRanges  = foldl combineRanges [] . sortBy (compare `on` fst)

combineRanges :: [XRange] -> XRange -> [XRange]
combineRanges [] x = [x]
combineRanges current (nextStart, nextEnd)
  | nextStart <= previousEnd + 1 = init current ++ [(previousStart, max nextEnd previousEnd)]
  | otherwise = current ++ [(nextStart, nextEnd)]
  where
    (previousStart, previousEnd) = last current
