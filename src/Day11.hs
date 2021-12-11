module Day11 where

import Control.Monad.State
import qualified Data.Matrix as M
import Data.Char
import Control.Monad
import Data.Ix

type Octopuses a = State (M.Matrix Int) a

day11_1 :: [String] -> IO ()
day11_1 = print . runState (runMany 100) . parseInput

day11_2 :: [String] -> IO ()
day11_2 = print . runState runSync . parseInput

parseInput :: [String] -> M.Matrix Int
parseInput = let parseRow = map digitToInt
                 makeMatrix = M.fromLists
                in makeMatrix . map parseRow

runSync :: Octopuses Int
runSync = do
  o <- get
  if allEquals o then
      return 0
      else do
        runStep
        n <- runSync
        return $ 1 + n

allEquals :: M.Matrix Int -> Bool
allEquals m = all (== head arr) arr
  where arr = M.toList m

runMany :: Int -> Octopuses Int
runMany 0 = return 0
runMany n = do
  count <- runStep
  next <- runMany (n - 1)
  return $ count + next

runStep :: Octopuses Int
runStep = do
  incOctopuses
  propagateFlash
  count <- countFlashed
  resetFlashed
  return count

incOctopuses :: Octopuses ()
incOctopuses = do
  o <- get
  put $ fmap (+1) o

propagateFlash ::  Octopuses ()
propagateFlash = do
  o <- get
  let patch = makePatch o
      patched = applyPatch patch o
   in put  patched
  count <- countNewFlashes
  unless (count == 0) propagateFlash

makePatch :: M.Matrix Int -> M.Matrix Int
makePatch m = foldl patch initPatch tenPos
  where
    tenPos = [(x, y) | x <- [1..M.nrows m], y <- [1..M.ncols m], m M.! (x, y) == 10]
    initPatch = M.zero (M.nrows m) (M.ncols m)

patch :: M.Matrix Int -> (Int, Int) -> M.Matrix Int
patch m (i, j) = let coordinates = [(x + i, y + j) | x <- [-1..1], y <- [-1..1]]
                  in M.mapPos (\coord a -> if coord `elem` coordinates then a + 1 else a) m

applyPatch :: M.Matrix Int -> M.Matrix Int -> M.Matrix Int
applyPatch = M.elementwise (\p x -> if x < 10 then min 10 (p + x) else p + x)

countNewFlashes :: Octopuses Int
countNewFlashes = gets (length . filter (== 10) . M.toList)

countFlashed :: Octopuses Int
countFlashed = gets (length . filter (>= 10) . M.toList)

resetFlashed :: Octopuses ()
resetFlashed = do
  o <- get
  put $ fmap (\x -> if x > 9 then 0 else x) o
