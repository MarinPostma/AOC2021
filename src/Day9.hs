module Day9 (day9_1, day9_2)
  where
import Data.Char
import qualified Data.Matrix as M
import qualified Data.Set as S
import Data.List

day9_1 :: [String] -> IO ()
day9_1 input = print . sum $ map (+1) minVals
  where m = parseInput input
        minPos = findMins m
        minVals = map (\(i, j) -> M.getElem i j m) minPos

day9_2 :: [String] -> IO ()
day9_2 input = print $ product $ take 3 $ sortBy (flip compare) $ map S.size bassins
  where m = parseInput input
        mins = findMins m
        bassins = map (\(i, j) -> traversal i j m S.empty) mins

parseInput :: [String] -> M.Matrix Int
parseInput = M.fromLists . map (map digitToInt)

findMins :: M.Matrix Int -> [(Int, Int)]
findMins m = map fst $ filter (\((i, j), n) -> all (> M.getElem i j m) n) [((i, j), map fst $ neighbors i j (\x y -> (x, y)/= (0, 0)) m) | i <- [1..M.nrows m], j <- [1..M.ncols m]]

neighbors :: Int -> Int -> (Int -> Int -> Bool) -> M.Matrix Int -> [(Int, (Int, Int))]
neighbors i j f m = filterMap id [makePoint (i + x) (j + y) m | x <- [-1..1], y <- [-1..1], f x y]

makePoint i j m = case M.safeGet i j m of
       Just val -> Just (val, (i, j))
       Nothing -> Nothing

traversal :: Int -> Int -> M.Matrix Int -> S.Set (Int, Int) -> S.Set (Int, Int)
traversal i j m s = case M.getElem i j m of
        9 -> s
        other -> foldl traverseNeig (S.insert (i, j) s) $ neighbors i j (\x y -> abs x /= abs y) m
  where traverseNeig s (v, (i, j)) = if S.member (i, j) s || v == 9 then s else traversal i j m s

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap _ [] = []
filterMap f (x:xs) = case f x of
    Just y   -> y:filterMap f xs
    Nothing  -> filterMap f xs
