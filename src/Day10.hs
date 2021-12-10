module Day10 (day10_1, day10_2)
 where
import Data.List
import qualified Data.Map as M

data State = EOI | Corrupted Char | Ok deriving (Show)
data Cat = Opening Char | Closing Char deriving (Show)

cat c = if c `elem` "[{(<" then Opening c else Closing c

day10_1 :: [String] -> IO ()
day10_1 = print . sum . map (countPoints . fst . verify [])

day10_2 :: [String] -> IO ()
day10_2 input = print $ scores !! (length scores `div` 2)
  where scores = sort . map (computeCompletionScore 0 . snd) . filter (isEOI . fst) . map (verify []) $ input

verify :: [Char] -> String -> (State, [Char])
verify [] "" = (Ok, [])
verify stack "" = (EOI, stack)
verify stack (c:s) = case cat c of
                     Opening c -> verify (stack ++ [c]) s
                     Closing c -> if match (last stack) c then verify (init stack) s else (Corrupted c, stack)

countPoints (Corrupted c) = points M.! c
countPoints _ = 0

isEOI EOI = True
isEOI _ = False

match '[' ']' = True
match '(' ')' = True
match '{' '}' = True
match '<' '>' = True
match _ _ = False

points = M.fromList [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]
score = M.fromList [('(', 1), ('[', 2), ('{', 3), ('<', 4)]

computeCompletionScore :: Int -> [Char] -> Int
computeCompletionScore score [] = score
computeCompletionScore s stack  = computeCompletionScore (5 * s + score M.! last stack) (init stack)
