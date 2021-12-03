{-# LANGUAGE TypeApplications #-}

import Data.Bifunctor

import Data.Char as C
import Data.List as L
import System.Environment

increaseBit (b0, b1) 0 = (b0 + 1, b1)
increaseBit (b0, b1) 1 = (b0, b1 + 1)
pickBest (b0, b1) = if b1 >= b0 then 1 else 0

mostCommon = fmap (pickBest . L.foldl' increaseBit (0, 0)) . L.transpose
leastCommon = fmap (1 -)

toDec = L.foldl' (\n b -> n * 2 + b) 0

type Bin = [Int]
type BinMat = [[Int]]

solve1 :: BinMat -> Int
solve1 xs = gamma * epsilon
 where
  t = mostCommon xs
  gamma = toDec t
  epsilon = toDec $ leastCommon t

{- Part two -}
go :: [(Bin, Bin)] -> (BinMat -> Bin) -> Bin
go [(first, _)] criteria = first
go xs criteria = go nextList criteria
 where
  nextBit = head . criteria . fmap snd $ xs
  nextList = fmap (second tail) . filter ((nextBit ==) . head . snd) $ xs

solve2 :: BinMat -> Int
solve2 xs = oxygen * co2
 where
  applyCriteria = toDec . go (fmap (\x -> (x, x)) xs)
  oxygen = applyCriteria mostCommon
  co2 = applyCriteria (leastCommon . mostCommon)

solutions =
  [ solve1
  , solve2
  ]

main :: IO ()
main = do
  idx <- read . head <$> getArgs
  print
    . (solutions !! (idx - 1))
    . fmap (fmap ((\x -> x - C.ord '0') . C.ord))
    . lines
    =<< getContents
