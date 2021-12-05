{-# LANGUAGE TupleSections #-}
module Day5 (day5_1, day5_2)
  where
import Data.List.Split
import qualified Data.Map as Map
import Control.Monad
import Control.Arrow
import Data.Bifunctor

data Point = Point { x, y :: Int } deriving (Show, Eq, Ord)
data Line = Line { p1, p2 :: Point } deriving (Show)

day5_1 :: [String] -> IO ()
day5_1  = print . countIntersect . filter isHorizontal . parseInput

day5_2 :: [String] -> IO ()
day5_2 = print . countIntersect . parseInput

countIntersect :: [Line] -> Int
countIntersect = length . Map.filter (>1) . Map.fromListWith (+) . map (, 1) . join . map drawLine

parseInput :: [String] -> [Line]
parseInput = map (parseLine . splitOn " -> ")

parseLine :: [String] -> Line
parseLine [p1, p2] = uncurry Line $ join bimap parsePoint (p1, p2)
parseLine _ = error "bad input"

parsePoint :: String -> Point
parsePoint s = Point p1 p2
  where [p1, p2] = map read $ splitOn "," s

drawLine :: Line -> [Point]
drawLine line
  | isHorizontal line = drawHorizontal line
  | otherwise = drawDiagonal line

drawDiagonal :: Line -> [Point]
drawDiagonal line = take nPoints [Point x y | (x, y) <- iterate (\(x, y) -> (x + xdir, y + ydir)) (x p1, y p1)]
  where Line { p1=p1, p2=p2 } = line
        dir a b = (b - a) `div` abs (a - b)
        xdir = dir (x p1) (x p2)
        ydir = dir (y p1) (y p2)
        nPoints = abs (x p1 - x p2) + 1

drawHorizontal line =  [Point x y | x <- [minx..maxx], y <- [miny.. maxy]]
  where minmax a b = (min a b, max a b)
        (minx, maxx) = minmax (x (p1 line)) (x (p2 line))
        (miny, maxy) = minmax (y (p1 line)) (y (p2 line))

isHorizontal :: Line -> Bool
isHorizontal line = x (p1 line) == x (p2 line) || y (p1 line) == y (p2 line)
