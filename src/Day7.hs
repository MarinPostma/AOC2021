module Day7 (day7_1, day7_2)
  where
import Control.Arrow
import Data.List.Split
import Data.List

day7_1 :: [String] -> IO ()
day7_1 = print . uncurry computeFuel . (median &&& id) . parseInput . head

day7_2 :: [String] -> IO ()
day7_2 = print . uncurry findMin . (median &&& id) . parseInput . head

findMin :: Int -> [Int] -> Int
findMin estimate crabs
  | estimateMoreFuel > estimateFuel && estimateLessFuel > estimateFuel = estimateFuel
  | estimateMoreFuel < estimateFuel = findMin (estimate + 1) crabs
  | estimateLessFuel < estimateFuel = findMin (estimate - 1) crabs
  | otherwise = error "invalid input"
  where estimateFuel = computeFuelAdj estimate crabs
        estimateMoreFuel = computeFuelAdj (estimate + 1) crabs
        estimateLessFuel = computeFuelAdj (estimate - 1) crabs

median :: [Int] -> Int
median =  uncurry (!!) . (id &&& ((`div` 2) . length)) . sort

parseInput :: String -> [Int]
parseInput = map read . splitOn ","

computeFuel :: Int -> [Int] -> Int
computeFuel align = sum . map (abs . (align -))

computeFuelAdj :: Int -> [Int] -> Int
computeFuelAdj align = sum . map (\x -> sum [1..(abs (align - x))])
