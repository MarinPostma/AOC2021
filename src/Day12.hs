{-# LANGUAGE RecordWildCards #-}
module Day12 (day12_1, day12_2) where

import Data.List.Split
import qualified Data.Map as M
import Control.Monad.State
import Data.List
import Data.Char
import qualified Data.Set as S
import Control.Monad

type CaveMap = M.Map String (S.Set String)

day12_1 :: [String] -> IO ()
day12_1 input = print $ length (visit cave False [] [] "start")
  where cave = parseInput M.empty input

day12_2 :: [String] -> IO ()
day12_2 input = print $ length (visit cave True [] [] "start")
  where cave = parseInput M.empty input

parseInput :: CaveMap -> [String] -> CaveMap
parseInput m [] = m
parseInput m (l:ss) = let [start, end] = splitOn "-" l
                          newMap = M.insertWith S.union start (S.singleton end) m
                          newMap2 = M.insertWith S.union end (S.singleton start) newMap
                          in parseInput newMap2 ss

visit :: CaveMap -> Bool -> [String] -> [String] -> String -> [[String]]
visit _ _ _ path "end" = [path ++ ["end"]]
visit m twice visited path next
  | all isLower next && next `elem` visited = []
  | otherwise = let nodes = M.lookup next m
                    newVisited = if all isUpper next then visited else next:visited
                 in case nodes of
                      Just nodes -> let visits = filter (not . null) $ join $ map (visit m twice newVisited (path ++ [next])) (S.toList nodes)
                                        double = if next /= "start" && twice then filter (not . null) $ join $ map (visit m False visited (path ++ [next])) (S.toList nodes) else []
                                     in nub $ double ++ visits
                      Nothing -> []
