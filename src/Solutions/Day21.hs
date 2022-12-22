module Solutions.Day21
  ( aoc21
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Text.Trifecta       (Parser, TokenParsing (token), count, integer, whiteSpace, CharParsing (anyChar))
import qualified Data.Map as M
import Text.Parser.Combinators (some, Parsing (try))
import Text.Parser.Char ( letter, char, string )
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Common.Debugging (traceLns)
import Text.Regex.TDFA.IntArrTrieSet (TrieSet(value))

aoc21 :: IO ()
aoc21 = do
  printSolutions 21 $ MkAoCSolution parseInput part1
  printSolutions 21 $ MkAoCSolution parseInput part2

data Operation = Plus | Minus | Times | Divide | Equals deriving (Enum, Show)
type Yell = Integer
type OpYell = (String, Operation, String)
data Monkey = Yell Integer | OpYell (String, Operation, String) deriving (Show)

parseInput :: Parser ( M.Map String Monkey)
parseInput = do
  monkeys <- some $ token parseMonkey
  pure $ M.fromList monkeys

parseMonkey :: Parser (String, Monkey)
parseMonkey = do
  name <- count 4 letter
  string ": "
  monkey <- try (Yell <$> integer) <|> try (OpYell <$> parseOpYell)
  pure ( name, monkey)

parseOpYell :: Parser OpYell
parseOpYell = do
  first <- count 4 letter
  whiteSpace
  operationChar <- anyChar
  operation <- parseOperation operationChar
  whiteSpace
  second <- count 4 letter
  pure (first, operation, second)
  where
    parseOperation c = case c of
      '+' -> pure Plus
      '-' -> pure Minus
      '/' -> pure Divide
      '*' -> pure Times
      _ -> fail "What are you doing?"


part1 :: M.Map String Monkey -> Maybe Integer
part1 = findSum "root"

findSum :: String -> M.Map String Monkey -> Maybe Integer
findSum currentMonkeyName allMonkeys = do
    currentMonkey <- M.lookup currentMonkeyName allMonkeys
    dependencies currentMonkey
    where
      dependencies m = case m of
        Yell value -> pure value
        OpYell (first, op, second) -> do
          firstValue <- findSum first allMonkeys
          secondValue <- findSum second allMonkeys
          pure $ combine firstValue secondValue op

combine :: Integer -> Integer -> Operation -> Integer
combine f s op = case op of
  Plus -> f + s
  Times -> f * s
  Minus -> f - s
  Divide -> f `div` s
  Equals -> f



part2 :: M.Map String Monkey -> Maybe Integer
part2 allMonkeys = do
    currentMonkey <- M.lookup "root" allMonkeys
    dependencies currentMonkey
    where
      dependencies m = case m of
        Yell value -> pure 0
        OpYell (f, _, s) -> expectedRootValue (f, Equals, s) 0 allMonkeys

expectedRootValue :: (String, Operation, String) -> Integer -> M.Map String Monkey -> Maybe Integer
expectedRootValue (f, op, s) currentSum monkeys = do
    otherValue <- if firstMonkeyContainsHuman then findSum s monkeys else findSum f monkeys
    let nextSum = newSum otherValue
    if nextNode == humanKey then pure nextSum else do
      nextChoice <- findNextChoice nextNode monkeys
      expectedRootValue nextChoice nextSum monkeys
    where firstMonkeyContainsHuman = containsHuman f monkeys
          nextNode = if firstMonkeyContainsHuman then f else s
          newSum v = case op of
                    Plus -> currentSum - v
                    Times -> currentSum `div` v
                    Minus -> if firstMonkeyContainsHuman then currentSum + v else v - currentSum
                    Divide -> if firstMonkeyContainsHuman then currentSum * v else v `div` currentSum
                    Equals -> v

findNextChoice :: String -> M.Map String Monkey -> Maybe (String, Operation, String)
findNextChoice key allMonkeys  = do
    currentMonkey <- M.lookup key allMonkeys
    dependencies currentMonkey
    where
      dependencies m = case m of
        Yell value -> Nothing
        OpYell v -> Just v


humanKey = "humn"

containsHuman :: String -> M.Map String Monkey -> Bool
containsHuman key allMonkeys
  | key == humanKey = True
  | otherwise = fromMaybe False checkMonkey
  where
    checkMonkey = do
      currentMonkey <- M.lookup key allMonkeys
      pure $ checkKids currentMonkey
    checkKids m = case m of
        Yell value ->  False
        OpYell (first, _, second) ->  containsHuman first allMonkeys || containsHuman second allMonkeys

