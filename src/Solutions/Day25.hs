module Solutions.Day25
    ( aoc25
    ) where

import           Common.AoCSolutions     (AoCSolution (MkAoCSolution),
                                          printSolutions, printTestSolutions)
import           Common.Debugging        (traceLns)
import qualified Data.Map                as M
import           Text.Parser.Char        (notChar)
import           Text.Parser.Combinators (some)
import           Text.Parser.Token       (token)
import           Text.Trifecta           (CharParsing (anyChar), Parser,
                                          newline, sepBy)

aoc25 :: IO ()
aoc25 = do
  printSolutions 25 $ MkAoCSolution parseInput part1

data SNAFUDigit = Two | One | Zero | Minus | DoubleMinus deriving
    ( Enum
    , Eq
    , Show
    )
type SNAFUNumber = [SNAFUDigit]

parseInput :: Parser [SNAFUNumber]
parseInput = do
  some $ token parseSNAFUNumber
  where
    parseSNAFUNumber = do
     some parseSNAFUDigit

parseSNAFUDigit :: Parser SNAFUDigit
parseSNAFUDigit = do
  c <- notChar '\n'
  parseChar c
  where
    parseChar char = case char of
            '2' -> pure Two
            '1' -> pure One
            '0' -> pure Zero
            '-' -> pure Minus
            '=' -> pure DoubleMinus
            _   -> fail "Nope"

part1 :: [SNAFUNumber] -> String
part1 = decimalToSnafu . sum . map snafuToDecimal

snafuToDecimal:: SNAFUNumber -> Integer
snafuToDecimal = sum . zipWith (\i d -> i * toMagnitude d) (iterate (5*) 1) . reverse

toMagnitude :: SNAFUDigit -> Integer
toMagnitude Two         = 2
toMagnitude One         = 1
toMagnitude Zero        = 0
toMagnitude Minus       = -1
toMagnitude DoubleMinus = -2

decimalToSnafu:: Integer -> String
decimalToSnafu i = map (digitToSnafu . (`M.lookup` snafuMap)) . reverse $ [0..(maximum (M.keys snafuMap))]
  where startingPower = floor $ logBase 5 (fromIntegral i)
        snafuMap = findSnafuMap M.empty startingPower i

digitToSnafu :: Maybe Integer ->  Char
digitToSnafu (Just 2)    = '2'
digitToSnafu (Just 1)    = '1'
digitToSnafu (Just 0)    = '0'
digitToSnafu (Just (-1)) = '-'
digitToSnafu (Just (-2)) = '='
digitToSnafu _           = '0'

findSnafuMap :: M.Map Integer Integer -> Integer -> Integer -> M.Map Integer Integer
findSnafuMap currentDigits (-1) _ = currentDigits
findSnafuMap currentDigits currentPower value
    | value <= 0 = currentDigits
    | otherwise = findSnafuMap updatedDigits (currentPower - 1) (value - (amountRequired * digitValue))
    where digitValue = floor $ 5 ^^ currentPower
          amountRequired = value `div` digitValue
          updatedDigits = normalise $ M.insert currentPower amountRequired currentDigits

normalise :: M.Map Integer Integer -> M.Map Integer Integer
normalise digits
  | not (any (> 2) $ M.elems digits) = digits
  | otherwise = normalise $ foldr sortIt digits (M.toList digits)

sortIt :: (Integer, Integer) -> M.Map Integer Integer -> M.Map Integer Integer
sortIt (dig, value) digits
  | value <= 2 = digits
  | otherwise = M.insertWith (+) (dig + 1) 1 reduced
    where reduced = M.insert dig (value - 5) digits


