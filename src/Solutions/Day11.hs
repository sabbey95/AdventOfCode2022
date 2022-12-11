module Solutions.Day11
    ( aoc11
    ) where

import           Common.AoCSolutions     (AoCSolution (MkAoCSolution),
                                          printSolutions, printTestSolutions)
import           Control.Applicative     ((<|>))
import           Control.Arrow           (Arrow (second))
import           Control.Concurrent      (newMVar)
import           Data.Bits               (Bits (xor))
import           Data.List               (partition, sort)
import qualified Data.Map                as M
import           Data.Maybe              (fromMaybe)
import           Text.Parser.Combinators (some, try)
import           Text.Parser.Token       (integer)
import           Text.Trifecta           (CharParsing (anyChar, string), Parser,
                                          Parsing (skipMany), commaSep, many,
                                          optional, whiteSpace)
import Common.Debugging (traceLns)

aoc11 :: IO ()
aoc11 = do
  printSolutions 11 $ MkAoCSolution parseInput part1
  printSolutions 11 $ MkAoCSolution parseInput part2

data Combinator = Plus | Times deriving (Enum, Show)
data Operation
  = Operation
      { combinator :: Combinator
      , numbers    :: (Maybe Integer, Maybe Integer)
      }
  deriving (Show)

data Test
  = Test
      { divisible :: Integer
      , ifTrue    :: Integer
      , ifFalse   :: Integer
      }
  deriving (Show)

data Monkey
  = Monkey
      { monkeyId        :: Integer
      , items           :: [Integer]
      , inspectionCount :: Integer
      , operation       :: Operation
      , test            :: Test
      }
  deriving (Show)

parseInput :: Parser [Monkey]
parseInput = do
  some parseMonkey

parseMonkey :: Parser Monkey
parseMonkey = do
  string "Monkey "
  id <- integer
  string ":"
  whiteSpace
  string "Starting items: "
  items <- commaSep integer
  operation <- parseOperation
  Monkey id items 0 operation <$> parseTest

parseOperation:: Parser Operation
parseOperation = do
    whiteSpace
    string "Operation: new = "
    first <- parseInt
    combCar <- anyChar
    comb <- parseComb combCar
    whiteSpace
    second <- parseInt
    pure $ Operation comb (first, second)
    where
      parseComb l =
        case l of
          '+' -> pure Plus
          '*' -> pure Times
          _   -> fail "Unrecognised character"

parseInt:: Parser (Maybe Integer)
parseInt = do
  first <- optional integer
  skipMany $ string "old"
  whiteSpace
  pure first

parseTest::Parser Test
parseTest = do
  whiteSpace
  string "Test: divisible by "
  div <- integer
  whiteSpace
  string "If true: throw to monkey "
  ifTrue <- integer
  whiteSpace
  string "If false: throw to monkey "
  Test div ifTrue <$> integer

part1 :: [Monkey] -> Integer
part1 = worryLevel 20 True

worryLevel :: Integer -> Bool -> [Monkey] ->  Integer
worryLevel rounds divBy3 = product . take 2 . reverse . sort . map inspectionCount . M.elems . runRound rounds divBy3 . mapMonkeys

runRound :: Integer -> Bool -> M.Map Integer Monkey -> M.Map Integer Monkey
runRound 0 _ monkeys = monkeys
runRound n divBy3 monkeys = foldr round monkeys [1..n]
  where keys = sort $ M.keys monkeys
        round r map = doThrows keys map divBy3

doThrows :: [Integer] -> M.Map Integer Monkey -> Bool -> M.Map Integer Monkey
doThrows [] monkeys _ = monkeys
doThrows (id:others) monkeys divBy3 = doThrows others newM divBy3
  where newM = handleMonkey (M.lookup id monkeys) monkeys divBy3

handleMonkey :: Maybe Monkey -> M.Map Integer Monkey -> Bool ->  M.Map Integer Monkey
handleMonkey Nothing monkeys _ = monkeys
handleMonkey (Just (Monkey id items inspectionCount operation (Test divisible ifTrue ifFalse))) monkeys divBy3=  M.update incrementCount id withFalses
  where operatedItems = map (performOperation operation) items
        adjustedItems = if divBy3 then map (`div` 3) operatedItems else map (`mod` modder monkeys) operatedItems
        (trues, falses) = partition (\c -> c `mod` divisible == 0) adjustedItems
        withTrues = M.update (addItems trues) ifTrue monkeys
        withFalses = M.update (addItems falses) ifFalse withTrues

modder:: M.Map Integer Monkey -> Integer
modder = product. map (divisible . test) . M.elems

addItems :: [Integer] -> Monkey -> Maybe Monkey
addItems newItems (Monkey id items inspectionCount operation (Test divisible ifTrue ifFalse)) =
  Just $ Monkey id (items ++ newItems) inspectionCount operation (Test divisible ifTrue ifFalse)

incrementCount :: Monkey -> Maybe Monkey
incrementCount (Monkey id items inspectionCount operation (Test divisible ifTrue ifFalse)) =
  Just $ Monkey id [] (inspectionCount + toInteger (length items)) operation (Test divisible ifTrue ifFalse)

performOperation :: Operation -> Integer -> Integer
performOperation (Operation combinator (first, second)) old = case combinator of
  Plus  -> trueFirst + trueSecond
  Times -> trueFirst * trueSecond
  where trueFirst = fromMaybe old first
        trueSecond = fromMaybe old second


mapMonkeys:: [Monkey] -> M.Map Integer Monkey
mapMonkeys = M.fromList . map (\c -> (monkeyId c, c))

part2 :: [Monkey] -> Integer
part2 = worryLevel 10000 False
