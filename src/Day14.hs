{-# LANGUAGE TupleSections #-}
module Day14 where

import qualified Data.Map as M
import Data.List.Split
import Data.List
import Control.Monad
import Control.Monad.State
import Control.Arrow
import Debug.Trace

type DistMap = M.Map Char Integer
type AssocMap = M.Map String Char
type AssocCount = M.Map String Integer
type Result a = State DistMap a

day14_1 :: [String] -> IO ()
day14_1 input = print $ uncurry (-) $ minmax $ M.insertWith (+) (last pat) 1 counts
  where (pat, m) = parseInput input
        ini = M.fromListWith (+) $  map (\(a, b) -> ([a, b], 1)) $ zip pat (drop 1 pat)
        counts = countFirst $ last $ take 41 $ iterate (solve m) ini

minmax = (maximum &&& minimum) . M.elems
countFirst :: AssocCount -> DistMap
countFirst = M.fromListWith (+) . map (\([a, _], v) -> (a, v)) . M.assocs

dist :: String -> DistMap
dist = foldl (\m x -> M.insertWith (+) x 1 m) M.empty

parseInput :: [String] -> (String, M.Map String Char)
parseInput (template:_:xs) = (template, pairs)
  where pairs = M.fromList $ map split xs
        split s = let [a, b] = splitOn " -> " s
                  in (a, head b)
parseInput _ = error "invalid input"

mapKeys :: AssocMap -> (String, Integer) -> [(String, Integer)]
mapKeys m (s@[a, b], n) = case M.lookup s m of
      Just c -> [([a, c], n), ([c, b], n)]
      Nothing -> [(s, n)]
mapKeys _ _ = error "bad input"

solve :: AssocMap -> AssocCount -> AssocCount
solve m c = M.fromListWith (+) $ join $ map (mapKeys m) $ M.assocs c
