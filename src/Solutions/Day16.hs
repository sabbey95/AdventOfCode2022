{-# LANGUAGE BlockArguments #-}
module Solutions.Day16
    ( aoc16
    ) where

import           Common.AoCSolutions             (AoCSolution (MkAoCSolution),
                                                  printSolutions,
                                                  printTestSolutions)
import           Control.Applicative.Combinators (skipManyTill)
import qualified Data.Graph                      as T
import qualified Data.Map                        as M
import Text.Parser.Char
    ( letter, newline, CharParsing(char, anyChar, string) )
import           Text.Parser.Combinators         (count, some)
import           Text.Parser.Token               (integer)
import           Text.Trifecta                   (DeltaParsing (position),
                                                  Parser, TokenParsing (token),
                                                  commaSep, optional)
import qualified Data.Set as S
import qualified GHC.Generics as S
import Common.Debugging (traceLns)
import Text.Regex.TDFA.Common (thd3)
import qualified Data.IntSet.Lens as M
import Debug.Trace (trace)

aoc16 :: IO ()
aoc16 = do
  -- printTestSolutions 16 $ MkAoCSolution parseInput part1
  printSolutions 16 $ MkAoCSolution parseInput part2

data Valve
  = Valve
      { id       :: String
      , flowRate :: Integer
      , exits    :: [String]
      }
  deriving (Show)

type FlowRates = M.Map String Integer
type Valves = M.Map String [String]
type StateMap = M.Map (String, String, Integer) Integer

parseInput :: Parser [Valve]
parseInput = do
  some parseValve
  where
    parseValveId = do
      char ' '
      count 2 letter
    parseExit = do
      optional $ char ','
      char ' '
      count 2 letter
    parseValve = do
      string "Valve"
      id <- parseValveId
      string " has flow rate="
      flowRate <- integer
      skipManyTill anyChar $ string "valve"
      optional $ char 's'
      exits <- some parseExit
      optional newline
      pure $ Valve id flowRate exits


part1 :: [Valve] -> Integer
part1 v = calculateMaxPressure (toValves v) (toFlowRates v) 29 "AA" "AA" S.empty 0

toFlowRates:: [Valve] -> FlowRates
toFlowRates = M.fromList . map (\(Valve id flowRate _) -> (id, flowRate))

toValves:: [Valve] -> Valves
toValves = M.fromList . map (\(Valve id _ exits) -> (id, exits))

calculateMaxPressure ::  Valves -> FlowRates -> Integer -> String -> String -> S.Set String -> Integer -> Integer
calculateMaxPressure valves flowRates timeRemaining last position openValves totalValue
   | timeRemaining <= 0 = totalValue
   | S.member position openValves || (flowRate == 0) = if null movements then 0 else maximum movements
   | otherwise = maximum (calc position (S.insert position openValves) (timeRemaining * flowRate + totalValue) : movements)
   where
    exits = filter (/=last) $ M.findWithDefault [] position valves
    flowRate = M.findWithDefault 0 position flowRates
    calc p open total = calculateMaxPressure valves flowRates (timeRemaining - 1) position p open total
    movements = map (\exit -> calc exit openValves totalValue) exits

part2 :: [Valve] -> Integer
part2 v =  calculateMaxPressure2 (toValves v) (toFlowRates v) 25 ("AA", "AA") S.empty 0 ("AA", "AA")

calculateMaxPressure2 ::  Valves -> FlowRates -> Integer -> (String, String) -> S.Set String ->Integer ->  (String, String)  -> Integer
calculateMaxPressure2 valves flowRates timeRemaining (meLast, elephantLast) openValves totalValue (mePosition, elephantPosition)
   | timeRemaining <= 0 = totalValue
   | otherwise = foldr calc 0 newPositions
   where
    flowRate p = M.findWithDefault 0 p flowRates
    shouldTurnValve p = S.notMember p openValves && (flowRate p /= 0)
    meValveTurns = [mePosition | shouldTurnValve mePosition]
    elephantValveTurns = [elephantPosition | elephantPosition /= mePosition && shouldTurnValve elephantPosition]

    newOpen (me, elephant) = do
      let withMe = if me == mePosition then S.insert me openValves else openValves
      if elephant == elephantPosition then S.insert elephant withMe else withMe

    newTotal (me, elephant) = (if me == mePosition then timeRemaining * flowRate me + totalValue else totalValue) +
        (if elephant == elephantPosition then timeRemaining * flowRate elephant  else 0)

    maxEm next total = do 
        traceLns (show $ max total next)
         max total next

    calc p total = do
      let next =  calculateMaxPressure2 valves flowRates (timeRemaining - 1) (mePosition, elephantPosition) (newOpen p) (newTotal p) p
      if next > 2740 then maxEm next total else max total next  


    elephantExits = filter (/=elephantLast) $ M.findWithDefault [] elephantPosition valves ++ elephantValveTurns
    meExits = filter (/=meLast) $ M.findWithDefault [] mePosition valves ++ meValveTurns
    newPositions = filter (/=(elephantPosition, mePosition)) [ (x,y) | x<-meExits, y<-elephantExits]



