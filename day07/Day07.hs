{-# LANGUAGE TypeApplications #-}

import Data.Char as C
import Data.Function
import qualified Data.List as L
import qualified Data.Map.Strict as M

import System.Environment

solveGen :: (Int -> Int) -> [Int] -> Int
solveGen f ls =
  minimum . map (\x -> sum . map (f . abs . (x -)) $ ls) $ [0 .. maximum ls]

solve1 = solveGen id

gauss n = n * (n + 1) `div` 2
solve2 = solveGen gauss

solutions =
  [ solve1
  , solve2
  ]

numbers =
  map (read @Int)
    . filter (C.isNumber . head)
    . L.groupBy ((==) `on` C.isNumber)

main :: IO ()
main = do
  idx <- read . head <$> getArgs
  print
    . (solutions !! (idx - 1))
    . numbers
    =<< getContents
