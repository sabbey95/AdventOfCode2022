module Solutions.Day22
    ( aoc22
    ) where

import           Common.AoCSolutions     (AoCSolution (MkAoCSolution),
                                          printSolutions, printTestSolutions)
import           Common.Debugging        (traceLns)
import           Control.Applicative     ((<|>))
import           Data.Function           (on)
import           Data.List               (elemIndex, maximumBy, minimumBy)
import qualified Data.Map                as M
import           Data.Maybe              (fromMaybe, isNothing, mapMaybe)
import qualified Data.Set                as S
import           Text.Parser.Combinators (some, try)
import           Text.Parser.Token       (token)
import           Text.Trifecta           (CharParsing (anyChar), Parser, count,
                                          integer', manyTill, newline)
import Data.Foldable (find)

aoc22 :: IO ()
aoc22 = do
  printSolutions 22 $ MkAoCSolution parseInput part1
  printSolutions 22 $ MkAoCSolution parseInput part2

type Move = Integer
type Point = (Integer, Integer)
type Line = (Point, Point)
data Turn = R | L deriving (Enum, Eq, Show)
data Space = Air | Wall deriving (Enum, Eq, Show)
type Board = M.Map Point Space
data Direction = N | E | S | W deriving (Enum, Eq, Show, Ord)
data Command
  = Move Move
  | Turn Turn
  deriving (Show)
data Input
  = Input
      { board    :: Board
      , commands :: [Command]
      }
  deriving (Show)

type PointMappings = M.Map (Point, Direction) (Point, Direction)

parseInput :: Parser Input
parseInput = do
  lines <- manyTill (manyTill anyChar newline) newline
  Input (linesToBoard lines) <$> parseCommand

linesToBoard:: [String] -> Board
linesToBoard lines = M.fromList $ concatMap toPoints withRows
  where withColumns = map (zip [0..]) lines
        withRows = zip [0..] withColumns

toPoints :: (Integer, [(Integer, Char)]) -> [(Point, Space)]
toPoints (row, columns) = mapMaybe toPoint columns
  where toPoint:: (Integer, Char) -> Maybe (Point, Space)
        toPoint (column, char)
          = case char of
            '#' -> pure ((column, row), Wall)
            '.' -> pure ((column, row), Air)
            _   -> Nothing


parseCommand :: Parser [Command]
parseCommand = do
  some . token $ try (Move <$> integer') <|> try (Turn <$> parseTurn)
  where parseTurn:: Parser Turn
        parseTurn = do
          c <- anyChar
          case c of
            'R' -> pure R
            'L' -> pure L
            _   -> fail "Nope"

part1 :: Input -> Integer
part1 (Input board commands) = runCommands board commands (startingPosition, startingDirection) Nothing
  where startingPosition = minimumBy (compare `on` fst) . filter (\(_, y) -> y == 0) $ M.keys board
        startingDirection = E

runCommands :: Board -> [Command] -> (Point, Direction) -> Maybe PointMappings -> Integer
runCommands _ [] ((x, y), direction) _ = (1000 * (y+ 1)) + (4 * (x+1)) + value
  where
    value = case direction of
              N -> 3
              E -> 0
              S -> 1
              W -> 2
runCommands board (currentCommand:others) (point, direction) pointMappings
  = case currentCommand of
      Move distance -> runCommands board others (move distance (point, direction) board pointMappings) pointMappings
      Turn turn -> runCommands board others (point, updateDirection direction turn) pointMappings

move :: Integer -> (Point, Direction) -> Board -> Maybe PointMappings -> (Point, Direction)
move 0 state _ _ = state
move distance (p, direction) board pointMappings
  | M.findWithDefault Wall (fst nextPoint) board == Wall = (p, direction)
  | otherwise =  move (distance - 1) nextPoint board pointMappings
  where unwrappedNextPoint = moveForward 1 p direction
        nextType = M.lookup unwrappedNextPoint board
        nextPoint = if isNothing nextType then wrapPoint direction p pointMappings board else (unwrappedNextPoint, direction)

moveForward:: Integer -> Point -> Direction ->  Point
moveForward  i (x, y) direction = case direction of
                        N -> (x, y - i)
                        E -> (x + i, y)
                        S -> (x, y + i)
                        W -> (x - i, y)

wrapPoint :: Direction -> Point -> Maybe PointMappings -> Board -> (Point, Direction)
wrapPoint d (x, y) Nothing board = (p,d)
  where
    p = case d of
        N -> (x, maximum. map snd . filter (\(x2, _) -> x2 == x) $ M.keys board)
        E -> (minimum . map fst . filter (\(_, y2) -> y2 == y) $ M.keys board, y)
        S -> (x, minimum. map snd . filter (\(x2, _) -> x2 == x) $ M.keys board)
        W -> (maximum . map fst . filter (\(_, y2) -> y2 == y) $ M.keys board, y)
wrapPoint d p (Just pointMappings) _ = M.findWithDefault (p,d) (p,d) pointMappings


updateDirection :: Direction -> Turn -> Direction
updateDirection direction R = cycleDirection direction directions
updateDirection direction L = cycleDirection direction (reverse directions)

cycleDirection :: Direction -> [Direction] -> Direction
cycleDirection d directions = cycle directions!!(fromMaybe 0 (elemIndex d directions) + 1)

directions = [N, E, S, W]

part2 :: Input -> Integer
part2 (Input board commands) = runCommands board commands (startingPosition, startingDirection) (Just mappings)
  where startingPosition = minimumBy (compare `on` fst) . filter (\(_, y) -> y == 0) $ M.keys board
        startingDirection = E
        cubeLength = 50
        allTopLeftCorners = getAllTopLeftCorners board startingPosition cubeLength S.empty
        lines = S.unions (S.map (allLines cubeLength) allTopLeftCorners)
        edges = S.filter (isEdge lines) lines
        allCorners = S.union (S.map fst lines) (S.map snd lines)
        edgeCornerGroups = groupEdges allCorners
        concaveCorners = S.filter (\s -> length s == 3) edgeCornerGroups
        mappings = M.unions $ S.map (startFindMappings board edges edgeCornerGroups) concaveCorners

startFindMappings :: Board -> S.Set Line -> S.Set [Point] -> [Point] -> PointMappings
startFindMappings board lines cornerGroups group
  | length group /= 3 || length oppositePoints < 2 = M.empty
  | otherwise = findMappings board lines cornerGroups (head oppositePoints, oppositePoints!!1) S.empty
  where oppositePoints = filter (\p -> any (\p2 -> manhattanDistance p p2 == 2) group) group

findMappings :: Board -> S.Set Line -> S.Set [Point] -> (Point, Point) -> S.Set Point -> PointMappings
findMappings board lines cornerGroups (point1, point2) visitedPoints
  | null corner1 && null corner2 = mappings
  | otherwise = M.union mappings $ findMappings board lines cornerGroups (nextPoint1, nextPoint2) (S.insert point1 $ S.insert point2 visitedPoints)
  where lineForPoint1 = findLine point1 visitedPoints lines
        lineForPoint2 = findLine point2 visitedPoints lines
        endForPoint1 = if fst lineForPoint1 == point1 then snd lineForPoint1 else fst lineForPoint1
        endForPoint2 = if fst lineForPoint2 == point2 then snd lineForPoint2 else fst lineForPoint2

        line1Points = lineToPoints (point1, endForPoint1)
        line2Points = lineToPoints (point2, endForPoint2)

        zipped = if isPositive (point1, endForPoint1) == isPositive (point2, endForPoint2) then zip line1Points line2Points else zip line1Points $ reverse line2Points
        forwardMappings = makeMappings board (lineForPoint1, lineForPoint2) zipped
        backMappings = makeMappings board (lineForPoint2, lineForPoint1) $ map (\(a, b) -> (b,a)) zipped

        mappings = M.union (M.fromList forwardMappings) (M.fromList backMappings)

        corner1 = filter (/= endForPoint1) $ fromMaybe [] $ find (endForPoint1 `elem`) cornerGroups
        corner2 = filter (/= endForPoint2) $ fromMaybe [] $ find (endForPoint2 `elem`) cornerGroups

        nextPoint1 = if null corner1 then endForPoint1 else head corner1
        nextPoint2 = if null corner2 then endForPoint2 else head corner2

makeMappings:: Board -> (Line, Line) -> [(Point, Point)] -> [((Point, Direction), (Point, Direction))]
makeMappings board (line1, line2) = map (\(l1, l2) -> ((l1, findDirectionMoving board line1 l1), (l2, flipDirection $ findDirectionMoving board line2 l2)))

findDirectionMoving:: Board -> Line -> Point -> Direction
findDirectionMoving board ((_, y1), (_, y2)) (x, y) 
    | isHorizontal = if isNothing (M.lookup (x, y-1) board) then N else S
    | otherwise = if isNothing (M.lookup (x+1, y) board) then E else W
    where isHorizontal = y1 == y2

flipDirection:: Direction -> Direction
flipDirection N = S
flipDirection S = N
flipDirection W = E
flipDirection E = W


findLine:: Point -> S.Set Point -> S.Set Line -> Line
findLine p visited = fromMaybe (p, p) . find (\l -> (fst l == p || snd l == p) && (fst l `S.notMember` visited && snd l `S.notMember` visited))

isPositive:: Line -> Bool
isPositive ((x1, y1), (x2, y2)) = x2 > x1 || y2 > y1

lineToPoints:: Line -> [Point]
lineToPoints ((x1, y1), (x2, y2)) = [(x, y) | x <- [(min x1 x2)..(max x1 x2)], y <- [(min y1 y2)..(max y1 y2)]]

isEdge :: S.Set Line -> Line -> Bool
isEdge otherLines (s,e) = S.null $ S.filter (\(s2, e2) -> manhattanDistance s e2 == 1 && manhattanDistance e s2 == 1) otherLines

manhattanDistance:: Point -> Point -> Integer
manhattanDistance (x1, y1) (x2,y2) = abs (x1 -x2) + abs (y1 - y2)

allLines :: Integer -> Point -> S.Set Line
allLines cubeLength p = S.fromList [(p, topRight), (topRight, bottomRight), (bottomRight, bottomLeft), (bottomLeft, p)]
  where topRight = moveForward (cubeLength - 1) p E
        bottomLeft = moveForward (cubeLength - 1) p S
        bottomRight = moveForward (cubeLength - 1) bottomLeft E


getAllTopLeftCorners :: Board -> Point -> Integer -> S.Set Point -> S.Set Point
getAllTopLeftCorners board point cubeLength corners = foldr combineCorners newCorners validCorners
  where otherPotentialCorners = map (moveForward cubeLength point) directions
        validCorners = filter (`M.member ` board) . filter (`S.notMember` corners) $ otherPotentialCorners
        newCorners = S.insert point corners
        combineCorners p existing = S.union existing (getAllTopLeftCorners board p cubeLength existing)


groupEdges :: S.Set Point -> S.Set [Point]
groupEdges corners = S.map (\p -> S.toList $ S.filter (\p2 -> manhattanDistance p p2 <= 2) corners) corners
