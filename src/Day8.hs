module Day8 (day8_1, day8_2)
  where
import Data.List.Split
import Control.Arrow
import Control.Monad
import Data.Bifunctor
import Data.List
import Data.Maybe
import Data.Char
import qualified  Data.Map.Strict as M

data Entry = Entry { codex:: Codex, output :: [String] } deriving (Show)
type Codex = M.Map String Int

day8_1 :: [String] -> IO ()
day8_1 = print . length . filter (`elem` [1, 4, 7, 8]) . join . map (\e -> map (decode (codex e)) (output e)) . parseInput

day8_2 :: [String] -> IO ()
day8_2 = print . sum . map decodeEntry . parseInput

parseInput :: [String] -> [Entry]
parseInput = map (uncurry Entry . parseLine)
  where parseLine = parse . splitOn "|"
        parse [pat, value] = (makeCodex (map sort (splitOn " " pat)), filter (not . null) (splitOn " " value))
        parse _ = error "invalid input"

makeCodex :: [String] -> Codex
makeCodex xs = three
     where init = foldl decodePattern M.empty xs
           two = findTwo init xs
           six = findSix two xs
           nine = findNineZero six xs
           three = findThreeFive nine xs

decodeEntry :: Entry -> Int
decodeEntry e = read $ map (intToDigit . decode (codex e)) (output e)

decodePattern :: Codex -> String -> Codex
decodePattern codex input
  | length input == 2 = M.insert input 1 codex
  | length input == 3 = M.insert input 7 codex
  | length input == 4 = M.insert input 4 codex
  | length input == 7 = M.insert input 8 codex
  | otherwise         = codex

findTwo :: Codex -> [String] -> Codex
findTwo codex xs = M.insert two 2 codex
        where bestFreq = fst $ minimumBy (\a b -> compare (snd b) (snd a)) $ M.assocs (M.fromListWith (+) (zip (join xs) (repeat 1)))
              two = head . filter (notElem bestFreq) $ xs

findSix :: Codex -> [String] -> Codex
findSix codex xs = M.insert six 6 codex
  where Just eight = fmap fst $ find (\(_, v) -> v == 8) $ M.assocs codex
        Just one = fmap fst $ find (\(_, v) -> v == 1) $ M.assocs codex
        sixCandidates = filter (\x -> length x  == 6) xs
        Just six = find (\x -> sort (x `union` one) == eight) sixCandidates

findNineZero :: Codex -> [String] -> Codex
findNineZero codex xs = M.insert zero 0 (M.insert nine 9 codex)
  where reducedCandidates = filter (`M.notMember` codex) xs
        Just four = fmap fst $ find (\(_, v) -> v == 4) $ M.assocs codex
        Just eight = fmap (sort . fst) $ find (\(_, v) -> v == 8) $ M.assocs codex
        [x, y] = filter (\x -> length x  == 6) reducedCandidates
        zero = if sort (x `union` four) == eight then x else y
        nine = if zero == x then y else x

findThreeFive :: Codex -> [String] -> Codex
findThreeFive codex xs = M.insert five 5 (M.insert three 3 codex)
  where reducedCandidates = filter (`M.notMember` codex) xs
        Just two = fmap fst $ find (\(_, v) -> v == 2) $ M.assocs codex
        Just eight = fmap (sort . fst) $ find (\(_, v) -> v == 8) $ M.assocs codex
        [x, y] = filter (\x -> length x  == 5) reducedCandidates
        five = if sort (x `union` two) == eight then x else y
        three = if five == x then y else x

decode :: Codex -> String -> Int
decode codex v = val
  where
       Just val =  M.lookup (sort v) codex
