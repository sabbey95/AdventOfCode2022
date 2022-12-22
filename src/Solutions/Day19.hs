module Solutions.Day19
    ( aoc19
    ) where

import           Common.AoCSolutions     (AoCSolution (MkAoCSolution),
                                          printSolutions, printTestSolutions)
import           Common.Debugging        (traceLns)
import qualified Data.Map                as M
import           Data.Maybe              (mapMaybe)
import           Text.Parser.Combinators
import           Text.Trifecta           (CharParsing (string), Parser,
                                          TokenParsing (token), integer, characterChar)
import Data.IntMap (findWithDefault)
import qualified Data.Set as S

aoc19 :: IO ()
aoc19 = do
  printTestSolutions 19 $ MkAoCSolution parseInput part1
  -- printSolutions 19 $ MkAoCSolution parseInput part2


data OreType = Ore | Clay | Obsidian | Geode deriving (Eq, Ord, Show)

data Blueprint
  = Blueprint
      { blueprintId   :: Integer
      , oreRobot      :: Ores
      , clayRobot     :: Ores
      , obsidianRobot :: Ores
      , geodeRobot    :: Ores
      }
  deriving (Show)


type Ores = M.Map OreType Integer
type Robots = M.Map OreType Integer

type State = (Robots, Ores)

initialState = (toOres (1, 0, 0, 0), toOres (0,0,0,0)) :: State

parseInput :: Parser [Blueprint]
parseInput = do
  some $ token parseBlueprint

parseBlueprint :: Parser Blueprint
parseBlueprint = do
  string "Blueprint "
  id <- integer
  string ": Each ore robot costs "
  oreRobot <- integer
  string "ore. Each clay robot costs "
  clayRobot <- integer
  string "ore. Each obsidian robot costs "
  obsidianOre <- integer
  string "ore and "
  obsidianClay <- integer
  string "clay. Each geode robot costs "
  geodeOre <- integer
  string "ore and "
  geodeObsidian <- integer
  string "obsidian."
  pure $ Blueprint id (toOres (oreRobot, 0, 0, 0)) (toOres (clayRobot, 0, 0, 0)) (toOres (obsidianOre ,obsidianClay, 0, 0)) (toOres (geodeOre, 0, geodeObsidian, 0))

toOres:: (Integer, Integer, Integer, Integer) -> Ores
toOres (ore, clay, obsidian, geode) = M.fromList [(Ore, ore), (Clay, clay), (Obsidian, obsidian), (Geode, geode)]


part1 :: [Blueprint] -> Integer
part1 = sum . map (\bp -> blueprintId bp * runTurn 24 initialState bp S.empty)

runTurn :: Integer -> State -> Blueprint -> S.Set OreType -> Integer
runTurn (0) state _ _ = M.findWithDefault 0 Geode (snd state)
runTurn timeRemaining state bluePrint missedOps
  | S.size newMissedOps >= 4 = maxPurchases
  | otherwise = max maxPurchases $ runTurn (timeRemaining - 1) (doMining state state) bluePrint newMissedOps
  where (withRobots, newMissedOps) = availableStates state timeRemaining bluePrint missedOps
        maxPurchases = foldr ((\updatedState currentMax -> max currentMax $ runTurn (timeRemaining - 1) updatedState bluePrint S.empty) . doMining state) 0 withRobots


availableStates:: State -> Integer -> Blueprint -> S.Set OreType -> ([State], S.Set OreType)
availableStates state t (Blueprint _ oreRobot clayRobot obsidianRobot geodeRobot) missedOps = (map snd availablePurchases,S.union missedOps $ S.fromList purchasedOreTypes)
  where
        maxOreRequired = maximum (map (M.findWithDefault 0 Ore) [clayRobot, obsidianRobot, geodeRobot])

        shouldBuildOre =  M.findWithDefault 0 Ore (snd state) < t * (maxOreRequired - M.findWithDefault 0 Ore (fst state))
        shouldBuildClay = M.findWithDefault 0 Clay (snd state) < t * (M.findWithDefault 0 Clay obsidianRobot - M.findWithDefault 0 Clay (fst state))
        shouldBuildObsidian = M.findWithDefault 0 Obsidian (snd state) < t * (M.findWithDefault 0 Obsidian geodeRobot - M.findWithDefault 0 Obsidian (fst state))

        possiblePurchases = filter (\ (ty, _) -> ty `notElem` missedOps) . map fst $ filter snd [((Ore, oreRobot), shouldBuildOre), ((Clay, clayRobot), shouldBuildClay),((Obsidian, obsidianRobot), shouldBuildObsidian), ((Geode, geodeRobot), True)]

        availablePurchases = mapMaybe (makePurchase state) possiblePurchases
        purchasedOreTypes = map fst availablePurchases

makePurchase :: State -> (OreType, Ores) -> Maybe (OreType, State)
makePurchase (robots, ores) (oreType, costs)
  | any (<0) (M.elems afterPurchase) = Nothing
  | otherwise = Just (oreType, (M.update (\c -> Just $ c + 1) oreType robots , afterPurchase))
  where afterPurchase = M.unionWith (-) ores costs


doMining:: State -> State -> State
doMining (oldRobots, _) (robots, ores)
  = (robots, M.unionWith (+) oldRobots ores)


part2 :: [Blueprint] -> Integer
part2 = product .  map (\bp -> runTurn 32 initialState bp S.empty) . filter (\bp -> blueprintId bp >= 3)
