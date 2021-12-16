{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
module Day15 where

import qualified Data.Matrix as M
import qualified Data.Map as Map
import qualified Data.Heap as H
import Control.Monad.State
import Data.Char
import Data.List
import Data.Ord (comparing)

type Point = (Int, Int)
type Map = M.Matrix Int
data AstarState = AstarState { openSet:: H.MinPrioHeap Int Point, cameFrom :: Map.Map Point Point, gScore, fScore :: Map.Map Point Int }

day15_1 :: [String] -> IO ()
day15_1 input = print $ s - m M.! (1, 1)
  where m = parseInput input
        goal = (M.ncols m, M.nrows m)
        s = sum $ map (m M.!) $ aStar m (1, 1) goal

day15_2 :: [String] -> IO ()
day15_2 input = print $ s - m M.! (1, 1)
  where m = makeBig $ parseInput input
        goal = (M.ncols m, M.nrows m)
        s = sum $ map (m M.!) $ aStar m (1, 1) goal

incrementMat :: Int ->  M.Matrix Int -> M.Matrix Int
incrementMat inc = M.mapPos (\_ x -> if (x + inc) > 9  then (x+inc) `mod` 10 + 1 else x+inc)

incMat = [[0, 1, 2, 3, 4]
         ,[1, 2, 3, 4, 5]
         ,[2, 3, 4, 5, 6]
         ,[3, 4, 5, 6, 7]
         ,[4, 5, 6, 7, 8]]

makeBig :: M.Matrix Int -> M.Matrix Int
makeBig m = let makeRow = foldl1 (M.<|>) . map (`incrementMat` m)
                rows = map makeRow incMat
            in foldl1 (M.<->) rows

parseInput :: [String] -> Map
parseInput = M.fromLists . map (map digitToInt)

-- manhatan distance
h :: Point -> Point -> Int
h (x, y) (xg, yg) = abs x - xg + (abs y - yg)

aStar :: Map -> Point -> Point -> [Point]
aStar m start goal = let openSet = H.singleton (h start goal,start)
                         cameFrom = Map.empty -- Map.Map (Int, Int) (Int, Int)
                         gScore = Map.fromList [(start, uncurry M.getElem start m)]
                         fScore = Map.fromList [(start, h start goal)]
                         initState = AstarState openSet cameFrom gScore fScore
                         in evalState (aStarRun m goal) initState

aStarRun :: Map -> Point -> State AstarState [Point]
aStarRun m goal = do
  AstarState { openSet, cameFrom, gScore, fScore } <- get
  case H.view openSet of
    Nothing -> return []
    Just ((_, current@(xc, yc)), tail) -> if current == goal then
                                      return $ makeRoute cameFrom current
                                      else let newOpenSet = tail
                                               currentScore = gScore Map.! current
                                               neighbors = filter (\(x, y) -> inRange (1, M.ncols m) x && inRange (1, M.nrows m) y)
                                                   [(xc - 1, yc), (xc + 1, yc), (xc, yc - 1), (xc, yc + 1)]
                                               neighborsScores = filter (betterScore gScore) $ map (computeGscore currentScore m) neighbors
                                               newGscore = Map.union (Map.fromList neighborsScores) gScore
                                               newOpenSet' = H.union (H.fromList (map  (\(x, _) -> (h x goal, x)) neighborsScores)) newOpenSet
                                               newCameFrom = Map.union (Map.fromList (map ((, current) . fst) neighborsScores)) cameFrom
                                               newFscore = Map.union (Map.fromList neighborsScores) fScore
                                            in do
                                              put $ AstarState newOpenSet' newCameFrom newGscore newFscore
                                              aStarRun m goal

inRange (a, b) x = x >= a && x <= b

betterScore :: Map.Map Point Int -> (Point, Int) -> Bool
betterScore gScores (p, score) = case Map.lookup p gScores of
                                   Just oldScore -> score < oldScore
                                   Nothing -> True

computeGscore :: Int -> Map -> Point -> (Point, Int)
computeGscore currentScore m p = let pointScore = m M.! p
                                 in (p, currentScore + pointScore)

makeRoute :: Map.Map Point Point -> Point -> [Point]
makeRoute m goal = unfold [goal]
  where unfold s@(x:xs) = case Map.lookup x m of
                            Just np -> unfold (np:s)
                            Nothing -> s
