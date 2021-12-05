{-# LANGUAGE TypeApplications #-}

import Control.Applicative
import Data.Char as C
import Data.Function
import qualified Data.List as L
import qualified Data.Map.Strict as M

import System.Environment

step a b
  | a < b = a + 1
  | a > b = a - 1
  | otherwise = a

linePoints (x1, y1, x2, y2)
  | x1 == x2 && y1 == y2 =
    [(x2, y2)]
  | otherwise = (x1, y1) : linePoints (step x1 x2, step y1 y2, x2, y2)

addPoint = M.alter (\m -> ((+ 1) <$> m) <|> Just 1)

solveGen pred ls =
  length . filter (> 1) . M.elems . L.foldl' (flip addPoint) M.empty $
    linePoints =<< filter pred ls

isHV (x1, y1, x2, y2) = x1 == x2 || y1 == y2
solve1 = solveGen isHV

solve2 = solveGen (const True)

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
    . fmap (\[a, b, c, d] -> (a, b, c, d))
    . fmap numbers
    . lines
    =<< getContents
