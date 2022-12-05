module Solutions.Day5
    ( aoc5
    ) where

import qualified Combinatorics.Coin      as M
import           Common.AoCSolutions     (AoCSolution (MkAoCSolution),
                                          printSolutions, printTestSolutions)
import           Common.Debugging        (traceLns)
import qualified Data.Map                as M
import           Text.Parser.Combinators (some)
import           Text.Trifecta           (CharParsing (string, char, anyChar), Parser,
                                          TokenParsing (token), integer, newline, whiteSpace, letter)
import Text.Parser.Combinators (try, optional, manyTill)
import Control.Monad.Combinators (skipManyTill)
import Data.List.Split (chunk, chunksOf)

aoc5 :: IO ()
aoc5 = do
  printSolutions 5 $ MkAoCSolution parseInput part1
  printSolutions 5 $ MkAoCSolution parseInput part2

type Crates = M.Map Integer [Char]

data Input
  = Input
      { crates       :: Crates
      , instructions :: [Instruction]
      }  deriving (Show)

data Instruction
  = Instruction
      { amount :: Integer
      , from   :: Integer
      , to     :: Integer
      }
  deriving (Show)

parseInput :: Parser Input
parseInput = do
  subCrates <- manyTill parseCrates $ string " 1 "
  skipManyTill anyChar newline
  newline
  Input (M.unionsWith (++) subCrates) <$> some (token parseInstruction)

parseCrates :: Parser Crates
parseCrates = do
  str <- manyTill anyChar newline
  let columns = chunksOf 4 str
  let chars = map (\c -> [c!!1 | not (null c) && head c == '[']) columns
  pure $ M.fromList $ filter ( not . null. snd) $ zip [1..] chars

parseInstruction:: Parser Instruction
parseInstruction = do
  string "move "
  amount <- integer
  string "from "
  from <- integer
  string "to "
  Instruction amount from <$> integer

part1 :: Input -> String
part1 = solve True


part2 :: Input -> String
part2 = solve False

solve :: Bool -> Input -> String
solve b (Input crates i) = map (head . snd) . M.toAscList $ performInstructions crates b i

performInstructions:: Crates -> Bool -> [Instruction] -> Crates
performInstructions crates reverse [] = crates
performInstructions crates reverse (i:others) = performInstructions newCrates reverse others
  where newCrates = performInstruction crates reverse i

performInstruction:: Crates ->Bool ->  Instruction -> Crates
performInstruction crates rev (Instruction amount from to) = updatedTo
  where fromRow = M.findWithDefault [] from crates
        updatedFrom = M.update (Just . drop (fromInteger amount)) from crates
        toMove = take (fromInteger amount) fromRow
        newTo = if rev then reverse toMove else toMove
        updatedTo = M.update (\f -> Just $ newTo  ++ f ) to updatedFrom
