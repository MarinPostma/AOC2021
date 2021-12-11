{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Lib
    ( entry
    ) where
import System.Console.CmdArgs
import Data.List
import Control.Exception
import Data.Void
import Data.Char (digitToInt)
import Data.Ratio
import Data.Bits
import Numeric
import Data.Bifunctor
import Debug.Trace
import Control.Monad
import Control.Arrow
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9
import Day10
import Day11

newtype Aoc = Aoc { day :: String }
  deriving (Show, Data, Typeable)

aoc = Aoc { day = def &= help "The day you want to run" }

entry :: IO ()
entry = runCmd . day =<< cmdArgs aoc

runDay f = print . f . lines =<< getContents

runCmd :: String -> IO ()
runCmd "day1_1" = runDay $ day1 0 . map read
runCmd "day1_2" = runDay $ day1 2 . map read
runCmd "day2_1" = runDay day2_1
runCmd "day2_2" = runDay day2_2
runCmd "day3_1" = runDay day3_1
runCmd "day3_2" = runDay day3_2
runCmd "day4_1" = day4_1 . lines =<< getContents
runCmd "day4_2" = day4_2 . lines =<< getContents
runCmd "day5_1" = day5_1 . lines =<< getContents
runCmd "day5_2" = day5_2 . lines =<< getContents
runCmd "day6_1" = day6_1 . lines =<< getContents
runCmd "day6_2" = day6_2 . lines =<< getContents
runCmd "day7_1" = day7_1 . lines =<< getContents
runCmd "day7_2" = day7_2 . lines =<< getContents
runCmd "day8_1" = day8_1 . lines =<< getContents
runCmd "day8_2" = day8_2 . lines =<< getContents
runCmd "day9_1" = day9_1 . lines =<< getContents
runCmd "day9_2" = day9_2 . lines =<< getContents
runCmd "day10_1" = day10_1 . lines =<< getContents
runCmd "day10_2" = day10_2 . lines =<< getContents
runCmd "day11_1" = day11_1 . lines =<< getContents
runCmd "day11_2" = day11_2 . lines =<< getContents
runCmd _ = print "unknown day!"

day1 :: Int -> [Int] -> Int
day1 win input = length $ filter id (uncurry (<) <$> zip t (tail t))
  where t = map sum $ transpose $ map (($ input) . drop) [0..win]

day2_1 :: [String] -> Int
day2_1 input = product $ foldl f [0, 0] $ map words input
  where
    f [x, y] ["forward", n] = [x + read n, y]
    f [x, y] ["down", n] = [x, y + read n]
    f [x, y] ["up", n] = [x, y - read n]
    f _ cmd = error $ "invalid command" ++ cmd !! 1

day2_2 :: [String] -> Int
day2_2 input = product $ take 2 $ foldl f [0, 0, 0] $ map words input
  where
    f [x, y, aim] ["forward", n] = [x + read n, y + aim * read n, aim]
    f [x, y, aim] ["down", n] = [x, y, aim + read n]
    f [x, y, aim] ["up", n] = [x, y, aim - read n]
    f _ cmd = error $ "invalid command" ++ cmd !! 1

day3_1 :: [String] -> Int
day3_1 = uncurry (*) . join bimap readBin . findMostLeastCommon

day3_2 :: [String] -> Int
day3_2 input = product $ map (readBin . ($ input)) [filterBits 0 (fst . findMostLeastCommon), filterBits 0 (snd . findMostLeastCommon)]

readBin = fst . head . readInt 2 (`elem` "01") digitToInt

filterBits :: Int -> ([String] -> String) -> [String] -> String
filterBits _ _ [input] = input
filterBits i screen input = filterBits (succ i) screen (filter (\xs -> xs !! i == screen input !! i) input)

findMostLeastCommon :: [String] -> (String, String)
findMostLeastCommon = foldl appendBit ("", "") . avg . transpose . map f
  where f = map digitToInt
        avg = map $ \xs -> sum xs % length xs
        appendBit (g, e) x = if x >= 1 % 2 then (g ++ "1", e ++ "0") else (g ++ "0", e ++ "1")
        compute x = x * complement x
