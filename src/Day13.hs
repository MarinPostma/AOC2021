module Day13 where

import Data.List.Split
import Control.Monad.State
import Data.List

type Point = (Int, Int)
data FoldDir = Y Int | X Int deriving (Show)

day13_1 :: [String] -> IO ()
day13_1 input = print $ length foldedPoints
  where (points, folds) = execState (parseInput input) ([], [])
        foldedPoints = nub $ foldl applyFold points [head folds]

day13_2 :: [String] -> IO ()
day13_2 input = putStr s
  where (points, folds) = execState (parseInput input) ([], [])
        foldedPoints = nub $ foldl applyFold points folds
        s = showMap foldedPoints

showMap :: [Point] -> String
showMap points = let maxX = maximum $ map fst points
                     maxY = maximum $ map snd points
                     putPoint i j
                       | i == maxX = '\n'
                       | (i, j) `elem` points = '#'
                       | otherwise = '.'
                  in [putPoint i j |  j <- [0..maxY], i <- [0..maxX]]

applyFold :: [Point] -> FoldDir -> [Point]
applyFold points (X n) = map (\(x, y) -> min (-x + 2 * n, y) (x, y)) points
applyFold points (Y n) = map (\(x, y) -> min (x, -y + 2 * n) (x, y)) points

parseInput :: [String] -> State ([Point], [FoldDir]) ()
parseInput [] = return ()
parseInput (x:xs) = do
  (points, folds) <- get
  case splitOn "," x of
    [x, y] -> put (points ++ [(read x, read y)], folds)
    [""] -> return ()
    _ -> let  [_, _, s] = splitOn " " x
              [dir, n] = splitOn "=" s
              f = case dir of
               "x" -> X $ read n
               "y" -> Y $ read n
               _ -> error "bad input"
          in put (points, folds ++ [f])
  parseInput xs
