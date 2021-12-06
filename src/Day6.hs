module Day6 (day6_1, day6_2)
  where
import Data.List.Split
import Data.List
import Debug.Trace
import Control.Arrow
import qualified Data.Map as Map

type Population = Map.Map Int Int

day6_1 :: [String] -> IO ()
day6_1 = print . sum . Map.elems . runSim 80 . prepareInput . parseInput . head

day6_2 :: [String] -> IO ()
day6_2 = print . sum . Map.elems . runSim 256 . prepareInput . parseInput . head

parseInput :: String -> [Int]
parseInput = map read . splitOn ","

prepareInput :: [Int] -> Population
prepareInput input = Map.fromList $ map (head &&& length) ((group . sort) input)

runSim :: Int -> Population -> Population
runSim days fishes =  iterate runDay fishes !! days

runDay :: Population -> Population
runDay fishes = foldl simDay Map.empty (Map.assocs fishes)

simDay :: Population -> (Int, Int) -> Population
simDay newPop (days, count) = case days of
                0 -> Map.unionWith (+) newPop (Map.fromList [(8, count), (6,count)])
                _ -> Map.insertWith (+) (days - 1) count newPop
