module Solutions.Day2
    ( aoc2
    ) where

import           Common.AoCSolutions     (AoCSolution (MkAoCSolution),
                                          printSolutions)
import           Text.Parser.Combinators (some)
import           Text.Trifecta           (Parser, letter, newline, whiteSpace)

aoc2 :: IO ()
aoc2 = do
  printSolutions 2 $ MkAoCSolution parseInput part1
  printSolutions 2 $ MkAoCSolution parseInput part2

data Choice = Rock | Paper | Scissors deriving (Enum, Eq, Show)
data MyChoice = X | Y | Z deriving (Enum, Eq, Show)

data Input
  = Input
      { elf :: Choice
      , me  :: MyChoice
      }
  deriving (Show)

data Game
  = Game
      { elfChoice :: Choice
      , myChoice  :: Choice
      }
  deriving (Show)

parseInput :: Parser [Input]
parseInput = do
  some parseGame

parseGame:: Parser Input
parseGame = do
  elfLetter <- letter
  whiteSpace
  myLetter <- letter
  newline
  elfChoice <- parseElfLetter elfLetter
  Input elfChoice <$> parseMyLetter myLetter
  where
    parseElfLetter l =
      case l of
        'A' -> pure Rock
        'B' -> pure Paper
        'C' -> pure Scissors
        _   -> fail "Unrecognised character"
    parseMyLetter l =
      case l of
        'X' -> pure X
        'Y' -> pure Y
        'Z' -> pure Z
        _   -> fail "Unrecognised character"

part1 :: [Input] -> Integer
part1 = sum . map (scoreGame . toGame)

toGame :: Input -> Game
toGame (Input elf me) = Game elf myChoice
  where
        myChoice =
          case me of
            X -> Rock
            Y -> Paper
            Z -> Scissors

scoreGame:: Game -> Integer
scoreGame (Game elfChoice myChoice) = scoreChoice myChoice + findWinner (Game elfChoice myChoice)

findWinner  :: Game -> Integer
findWinner (Game elfChoice myChoice)
  | elfChoice == myChoice = 3
  | elfChoice == getLoser myChoice = 6
  | otherwise  = 0

getWinner:: Choice -> Choice
getWinner Rock     = Paper
getWinner Paper    = Scissors
getWinner Scissors = Rock

getLoser:: Choice -> Choice
getLoser Rock     = Scissors
getLoser Paper    = Rock
getLoser Scissors = Paper

scoreChoice:: Choice -> Integer
scoreChoice Rock     = 1
scoreChoice Paper    = 2
scoreChoice Scissors = 3

part2 :: [Input] -> Integer
part2 = sum . map scoreInput

scoreInput:: Input -> Integer
scoreInput (Input elf me) = scoreGame $ Game elf myChoice
    where
      myChoice =
        case me of
          X -> getLoser elf
          Y -> elf
          Z -> getWinner elf
