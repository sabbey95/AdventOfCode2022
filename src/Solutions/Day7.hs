module Solutions.Day7
    ( aoc7
    ) where

import qualified Combinatorics.Coin      as M
import           Common.AoCSolutions     (AoCSolution (MkAoCSolution),
                                          printSolutions, printTestSolutions)
import           Control.Applicative
import           Data.Functor            (($>))
import qualified Data.Map                as M
import           Data.Maybe              (catMaybes, isNothing)
import           Linear                  (normalize)
import           System.FilePath.Lens    (directory)
import           Text.Parser.Combinators (many, try)
import           Text.Trifecta           (CharParsing (anyChar, string), Parser,
                                          integer, manyTill, newline)
import Common.Debugging (traceLns)

aoc7 :: IO ()
aoc7 = do
  printSolutions 7 $ MkAoCSolution parseInput part1
  printSolutions 7 $ MkAoCSolution parseInput part2

type Dir = String
type File = (Integer, String)
data TreeNode
  = DirPath [Dir]
  | File File
  deriving (Eq, Show)

type CDCommand = Dir
type LSCommand = [TreeNode]
data Command
  = CDCommand CDCommand
  | LSCommand LSCommand
  deriving (Eq, Show)

parseInput :: Parser [Command]
parseInput = do
  some (try (CDCommand <$> parseCDCommand ) <|> try (LSCommand <$> parseLSCommand))

parseCDCommand:: Parser CDCommand
parseCDCommand = do
  string "$ cd "
  manyTill anyChar newline


parseLSCommand:: Parser LSCommand
parseLSCommand = do
  string "$ ls"
  newline
  many (try (DirPath <$> parseDir) <|> try (File <$> parseFile))
  where parseDir:: Parser [Dir]
        parseDir = do
          string "dir "
          base <- manyTill anyChar newline
          pure [base]
        parseFile:: Parser File
        parseFile = do
          size <- integer
          name <- manyTill anyChar newline
          pure (size, name)

part1 :: [Command] ->  Integer
part1 x = sum . filter (<=100000) $ M.elems directorySizes
  where fileTree = resolveFiles [] x
        directorySizes = sizeDirectories fileTree (M.keys fileTree)

resolveFiles:: [Dir] -> [Command] -> M.Map [Dir] [TreeNode]
resolveFiles _ [] = M.empty
resolveFiles currentDirectory (c:others)  = case c of
  CDCommand s -> resolveFiles (if s == ".." then init currentDirectory else currentDirectory ++ [s]) others
  LSCommand nodes ->  M.insertWith (++) currentDirectory (map(normalizeDirectoryNames currentDirectory) nodes) $ resolveFiles  currentDirectory others

normalizeDirectoryNames :: [Dir] -> TreeNode -> TreeNode
normalizeDirectoryNames currentDirectoy n = case n of
          DirPath d -> DirPath (currentDirectoy ++ d)
          File f    -> File f

sizeDirectories:: M.Map [Dir] [TreeNode] -> [[Dir]] -> M.Map [Dir] Integer
sizeDirectories _ [] = M.empty
-- The input can actually be resolved without account for out of order cd-ing.
-- This means we can just use catMaybes, otherwise we would need to account for some directories that aren't calculatable yet
sizeDirectories directoryContents (dir:others) = M.insert dir (sum $ catMaybes componentSizes) otherDirectorySizes
    where otherDirectorySizes = sizeDirectories directoryContents others
          thisDirectoryContents = M.findWithDefault [] dir  directoryContents
          componentSizes = findDirSizes thisDirectoryContents otherDirectorySizes

findDirSizes:: [TreeNode] -> M.Map [Dir] Integer -> [Maybe Integer]
findDirSizes nodes calculatedSizes = map mapNode nodes
  where mapNode:: TreeNode -> Maybe Integer
        mapNode n = case n of
          DirPath d      -> M.lookup d calculatedSizes
          File (size, _) -> Just size

part2 :: [Command] ->  Integer
part2 x = minimum . filter (>=mustDelete) $ M.elems directorySizes
  where fileTree = resolveFiles [] x
        directorySizes = sizeDirectories fileTree (M.keys fileTree)
        usedSpace = M.findWithDefault 0 ["/"] directorySizes
        requiredSpace = 30000000
        unusedSpace = 70000000 - usedSpace
        mustDelete = requiredSpace - unusedSpace