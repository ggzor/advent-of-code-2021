{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

import Data.Bifunctor (second)
import Data.Char as C
import Data.Function
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Ord

import System.Environment

type Row = Int
type Col = Int

data Board = Board
  { marked :: M.Map (Row, Col) (Int, Bool)
  , revIdx :: M.Map Int (Row, Col)
  }
  deriving (Show)

parseBoard board =
  let (m, idx) =
        unzip
          [ (((i, j), (value, False)), (value, (i, j)))
          | (row, i) <- zip board [0 ..]
          , (value, j) <- zip row [0 ..]
          ]
   in Board (M.fromList m) (M.fromList idx)

nextNumber :: Int -> Board -> Board
nextNumber n (Board m idx) = Board (fromMaybe m rep) idx
 where
  rep = (\idx -> M.adjust (second (const True)) idx m) <$> M.lookup n idx

scoreBoard (Board m _) =
  sum . fmap fst . filter (not . snd) $ M.elems m

getAxis (Board m _) axis =
  fmap (fmap snd)
    . L.groupBy ((==) `on` axis . fst)
    . L.sortOn (axis . fst)
    $ M.toAscList m

isWinning :: Board -> Bool
isWinning board = or (fmap (and . fmap snd) . getAxis board =<< [fst, snd])

firstWinner :: [Board] -> [Int] -> (Board, Int)
firstWinner boards (n : ns) =
  let next = fmap (nextNumber n) boards
      winner = L.find isWinning next
   in maybe (firstWinner next ns) (,n) winner

solve1 :: [Board] -> [Int] -> Int
solve1 boards movements = scoreBoard w * n
 where
  (w, n) = firstWinner boards movements

{- Part 2 -}

lastWinner :: [Board] -> [Int] -> (Board, Int)
lastWinner boards (m : ms) =
  let next = fmap (nextNumber m) boards
      allCompleted = all isWinning next
   in if all isWinning next
        then
          (,m) . snd . head
            . filter
              ( \(bold, bnew) ->
                  not (isWinning bold) && isWinning bnew
              )
            $ zip boards next
        else lastWinner next ms

solve2 :: [Board] -> [Int] -> Int
solve2 boards movements = scoreBoard w * n
 where
  (w, n) = lastWinner boards movements

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
  idx <- (read @Int) . head <$> getArgs
  ms <- numbers <$> getLine
  boards <-
    fmap parseBoard
      . filter (not . null . head)
      . L.groupBy ((==) `on` L.null)
      . fmap numbers
      . lines
      <$> getContents
  print $ (solutions !! (idx - 1)) boards ms
