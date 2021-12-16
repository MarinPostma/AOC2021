module Day12 (day12_1, day12_2) where

import Data.List.Split
import qualified Data.Map as M
import Data.List
import Data.Char
import qualified Data.Set as S

type CaveMap = M.Map String (S.Set String)

day12_1 :: [String] -> IO ()
day12_1 input = print $ visit cave True S.empty "start"
  where cave = parseInput M.empty input

day12_2 :: [String] -> IO ()
day12_2 input = print $ visit cave False S.empty "start"
  where cave = parseInput M.empty input

parseInput :: CaveMap -> [String] -> CaveMap
parseInput m [] = m
parseInput m (l:ss) = let [start, end] = splitOn "-" l
                          newMap = M.insertWith S.union start (S.singleton end) m
                          newMap2 = M.insertWith S.union end (S.singleton start) newMap
                          in parseInput newMap2 ss

visit :: CaveMap -> Bool -> S.Set String -> String -> Int
visit _ _ _ "end" = 1
visit m twice visited next = case M.lookup next m of
                  Just nodes -> sum $ map (\x -> if x == "start"  || (S.member x visited && twice) then 0 else visit m (S.member x visited || twice) (if all isUpper x then visited else S.insert x visited) x) (S.toList nodes)
                  Nothing -> 0
