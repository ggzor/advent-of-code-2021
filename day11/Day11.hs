import Data.Char as C
import Data.Function
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import System.Environment

type Point = (Int, Int)
type Board = M.Map Point Int

parseBoard :: [[Char]] -> Board
parseBoard b =
  M.fromList $
    [((y, x), C.digitToInt v) | (r, y) <- zip b [0 ..], (v, x) <- zip r [0 ..]]

neighbors :: Point -> Board -> [Point]
neighbors (y, x) b =
  [ (ny, nx)
  | dy <- [-1 .. 1]
  , dx <- [-1 .. 1]
  , not (dx == 0 && dy == 0)
  , let ny = y + dy
  , let nx = x + dx
  , 0 <= ny && ny < 100
  , 0 <= nx && nx < 100
  ]

step :: Board -> Board
step b = go (M.map (+ 1) b) S.empty
 where
  go b flashed =
    let next =
          filter (not . (`S.member` flashed)) . map fst . filter ((> 9) . snd) $
            M.toList b
        toIncrease = concatMap (`neighbors` b) next
     in if null next
          then M.map (\v -> if v > 9 then 0 else v) b
          else
            go
              (L.foldl' (flip (M.adjust (+ 1))) b toIncrease)
              (L.foldl' (flip S.insert) flashed next)

steps = flip (L.scanl' (&)) (repeat step)

{- Part one -}
solve1 = length . filter (== 0) . concatMap M.elems . take 100 . drop 1 . steps

{- Part two -}
solve2 =
  fst . head . dropWhile (any (/= 0) . M.elems . snd) . drop 1 . zip [0 ..] . steps

solutions =
  [ solve1
  , solve2
  ]

main :: IO ()
main = do
  idx <- read . head <$> getArgs
  print
    . (solutions !! (idx - 1))
    . parseBoard
    . lines
    =<< getContents
