module Day4 (day4_1, day4_2)
  where
import Data.List.Split
import Data.List
import Control.Arrow
import Data.Maybe


data Grid = Grid { marks :: [[Bool]], numbers :: [[Int]], gridId :: Int } deriving (Show)

data Game = Game { draws :: [Int], current :: Maybe Int, grids :: [Grid], winner :: Maybe (Int, Grid) } deriving (Show)

day4_1 :: [String] -> IO ()
day4_1 input = print $ number * computeUnmarkedSum winner
  where numbers = parseNumList $ head input
        Game { winner = Just(number, winner) } = play $ Game numbers Nothing (makeGrids input) Nothing

day4_2 :: [String] -> IO ()
day4_2 input = print $ number * computeUnmarkedSum win
  where numbers = parseNumList $ head input
        games = takeWhile (isJust . winner) $ drop 1 $ iterate play $ Game numbers Nothing (makeGrids input) Nothing
        Game { winner=Just(number, win) } = last games

parseNumList :: String -> [Int]
parseNumList = map read . splitOn ","

makeGrids :: [String] -> [Grid]
makeGrids input = zipWith parseGrid (chunksOf 6 $ drop 1 input) [0..]

play :: Game -> Game
play Game { draws=(x:xs), current=(Just current), grids=grids, winner=winner } = case find validateGrid grids of
                           Just grid  -> Game (x:xs) (Just current) (filter (\g -> gridId g /= gridId grid) grids) (Just (current, grid))
                           Nothing -> play $ Game xs (Just x) (playNumber x grids) winner
play Game { current=Nothing, draws=(x:xs), winner=winner, grids=grids } = play $ Game xs (Just x) (playNumber x grids) winner
play Game { draws = [], grids = grids, current=current } = Game [] current grids Nothing

playNumber x = map (markGrid x)

computeUnmarkedSum :: Grid -> Int
computeUnmarkedSum grid = sum $ map snd $ filter (not . fst) $ zip (concat (marks grid)) (concat (numbers grid))

markGrid :: Int -> Grid -> Grid
markGrid x grid = Grid m n (gridId grid)
  where (m, n) = unzip $ zipWith (checkRow x) (marks grid) (numbers grid)

checkRow :: Int -> [Bool] -> [Int] -> ([Bool], [Int])
checkRow x marks nums = unzip $ zipWith mark marks nums
  where mark m n = if n == x then (True, n) else (m, n)


parseGrid :: [String] -> Int -> Grid
parseGrid input = Grid  wins numbers
  where wins = replicate 5 $ replicate 5 False
        numbers = map parseGridNums $ drop 1 input
        parseGridNums = map read . filter (not . null) . splitOn " "

validateGrid :: Grid -> Bool
validateGrid = uncurry (||) . (any and . marks &&& any and . transpose . marks)
