module Solutions.Day13
    ( aoc13
    ) where

import           Common.AoCSolutions            (AoCSolution (MkAoCSolution),
                                                 printSolutions,
                                                 printTestSolutions)
import           Control.Applicative            ((<|>))
import           Data.Function                  (on)
import           Data.List                      (sortBy, sortOn)
import           Data.List.NonEmpty             (sortWith)
import           Data.Maybe                     (fromMaybe, isNothing)
import           Data.Ord                       (comparing)
import           Data.Text.Internal.Fusion.Size (compareSize)
import           Text.Parser.Combinators        (count, many, some, try)
import           Text.Parser.Token              (token)
import           Text.Trifecta                  (CharParsing (char), Parser,
                                                 commaSep, integer, newline,
                                                 optional)

aoc13 :: IO ()
aoc13 = do
  printSolutions 13 $ MkAoCSolution parseInput part1
  printSolutions 13 $ MkAoCSolution parseInputPart2 part2

type IntNode = Integer
data Node
  = IntNode IntNode
  | Packet [Node]
  deriving (Eq, Show)
type Packet = [Node]
type PacketPair = (Packet, Packet)

parseInput :: Parser [PacketPair]
parseInput = do
  some $ token parsePacketPair

parseInputPart2 :: Parser [Packet]
parseInputPart2 = do
  some $ token parsePacketPart2
  where
    parsePacketPart2 = do
      packet <- parsePacket
      optional newline
      optional newline
      pure packet

parsePacketPair :: Parser PacketPair
parsePacketPair = do
  first <- parsePacket
  newline
  second <- parsePacket
  optional $ count 2 newline
  pure (first, second)

parsePacket :: Parser Packet
parsePacket = do
  char '['
  nodes <- commaSep $ try (Packet <$> parsePacket) <|> try (IntNode <$> integer)
  char ']'
  pure nodes


part1 :: [PacketPair] -> Int
part1 = sum . map fst . filter (\(_, (l, r)) -> comparePacketsWithDefault l r) . zip [1..]

comparePacketsWithDefault:: Packet -> Packet -> Bool
comparePacketsWithDefault l r = fromMaybe False $ comparePackets l r

comparePackets:: Packet -> Packet -> Maybe Bool
comparePackets [] [] = Nothing
comparePackets [] _ = Just True
comparePackets _ [] = Just False
comparePackets (left:otherLeft) (right:otherRight) |
  isNothing nodeComparison = comparePackets otherLeft otherRight
  | otherwise = nodeComparison
  where nodeComparison =  compareNodes left right

compareNodes :: Node -> Node -> Maybe Bool
compareNodes (IntNode l) (IntNode r) = if l == r then Nothing else Just $ l < r
compareNodes (Packet l) (Packet r)   = comparePackets l r
compareNodes l (Packet r)            = comparePackets [l] r
compareNodes (Packet l) r            = comparePackets l [r]

part2 :: [Packet] -> Int
part2 packets = product .map fst .filter (\(_, p) -> p `elem` dividers) $ zip [1..] sortedPackets
  where allPackets = packets ++ dividers
        sortedPackets = sortBy comparator allPackets

comparator :: Packet -> Packet -> Ordering
comparator l r = if correctOrder then LT else GT
  where correctOrder = comparePacketsWithDefault l r

dividers:: [Packet]
dividers = [[Packet [IntNode 2]], [Packet [IntNode 6]]]
