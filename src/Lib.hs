{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Lib
    ( entry
    ) where
import System.Console.CmdArgs
import Data.List
import Control.Exception
import Data.Void

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
