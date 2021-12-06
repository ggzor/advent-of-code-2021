{-# LANGUAGE TypeApplications #-}

import Control.Applicative
import Control.Monad
import Data.Char as C
import Data.Function
import Data.Functor
import qualified Data.List as L
import qualified Data.Map.Strict as M

import System.Environment

step (0, v) = [(8, v), (6, v)]
step (n, v) = [(n - 1, v)]

increaseItemBy x = M.alter (\v -> (v <&> (+ x)) <|> Just x)

multiplyLanternfish :: M.Map Int Int -> M.Map Int Int
multiplyLanternfish =
  (step <=< M.toList)
    <&> L.foldl' (\m (k, v) -> increaseItemBy v k m) M.empty

solveGen :: [Int] -> [Int]
solveGen ls =
  let initial = L.foldl' (flip $ increaseItemBy 1) M.empty ls
   in sum . M.elems <$> L.scanl' (&) initial (repeat multiplyLanternfish)

solve1 = (!! 80) . solveGen
solve2 = (!! 256) . solveGen

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
