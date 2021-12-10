import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe

import System.Environment

data State = Partial [Char] | Incorrect Char deriving (Show)

incorrectChar (Incorrect c) = Just c
incorrectChar _ = Nothing

remaining (Partial l) = Just l
remaining _ = Nothing

starting = "([{<"
ending = ")]}>"
matching = M.fromList $ zip ending starting ++ zip starting ending

parse :: String -> State
parse = L.foldl' step (Partial [])
 where
  step (Partial cs) new | new `elem` starting = Partial (new : cs)
  step (Partial []) new = Incorrect new
  step (Partial (c : rest)) new =
    if c == (matching M.! new)
      then Partial rest
      else Incorrect new
  step x _ = x

scores1 = M.fromList $ zip ending [3, 57, 1197, 25137]
solve1 :: [String] -> Int
solve1 = sum . map (scores1 M.!) . mapMaybe (incorrectChar . parse)

{- Part two -}

scores2 = M.fromList $ zip ending [1 ..]
solve2 :: [String] -> Int
solve2 = getMid . L.sort . map scoreLine . mapMaybe (remaining . parse)
 where
  scoreLine = L.foldl' (\a b -> a * 5 + b) 0 . map ((scores2 M.!) . (matching M.!))
  getMid l = l !! (length l `div` 2)

solutions =
  [ solve1
  , solve2
  ]

main :: IO ()
main = do
  idx <- read . head <$> getArgs
  print
    . (solutions !! (idx - 1))
    . lines
    =<< getContents
