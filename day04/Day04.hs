{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

import Data.Bifunctor
import Data.Char as C
import Data.Function (on)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)

import System.Environment

type Mat = [[Int]]
type Movements = [Int]

data Board
  = Incomplete
      { marked :: M.Map (Int, Int) (Int, Bool)
      , revIdx :: M.Map Int (Int, Int)
      }
  | Complete {score :: Int}
  deriving (Show)

isCompleted (Complete _) = True
isCompleted _ = False

parseBoard mat =
  let (marked, revIdx) =
        unzip
          [ (((i, j), (value, False)), (value, (i, j)))
          | (row, i) <- zip mat [0 ..]
          , (value, j) <- zip row [0 ..]
          ]
   in Incomplete (M.fromList marked) (M.fromList revIdx)

nextStep :: Int -> Board -> Board
nextStep _ b@(Complete _) = b
nextStep m (Incomplete marked revIdx) =
  let newMarked = flip (M.adjust (second (const True))) marked <$> M.lookup m revIdx
      new = Incomplete (fromMaybe marked newMarked) revIdx
   in if isWinning new then Complete (scoreBoard new) else new

scoreBoard :: Board -> Int
scoreBoard b@(Complete score) = score
scoreBoard (Incomplete marked _) =
  sum
    . fmap (fst . snd)
    . filter (not . snd . snd)
    $ M.toList marked

isWinning :: Board -> Bool
isWinning (Complete _) = True
isWinning (Incomplete marked _) =
  let rows = L.groupBy ((==) `on` fst . fst) . L.sortOn (fst . fst) $ M.toAscList marked
      cols = L.groupBy ((==) `on` snd . fst) . L.sortOn (snd . fst) $ M.toAscList marked
      result = or . fmap (and . fmap (snd . snd)) $ (rows ++ cols)
   in result

firstWinning :: [Board] -> Movements -> (Board, Int)
firstWinning boards (m : ms) =
  let next = fmap (nextStep m) boards
      winner = L.find isCompleted next
   in maybe (firstWinning next ms) (,m) winner

solve1 :: [Board] -> Movements -> Int
solve1 boards movements = score * movement
 where
  (Complete score, movement) = firstWinning boards movements

{- Part 2 -}

lastWinning :: [Board] -> Movements -> (Board, Int)
lastWinning boards (m : ms) =
  let next = fmap (nextStep m) boards
      allCompleted = all isCompleted next
   in if all isCompleted next
        then
          (,m) . snd . head
            . filter
              ( \(bold, bnew) ->
                  not (isCompleted bold) && isCompleted bnew
              )
            $ zip boards next
        else lastWinning next ms

solve2 :: [Board] -> Movements -> Int
solve2 boards movements = score * movement
 where
  (Complete score, movement) = lastWinning boards movements

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
