{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}

import Control.Monad.Reader
import Data.Char as C
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S

import System.Environment

type Point = (Int, Int)
type Cells = M.Map Point Int
data Board = Board !Int !Int Cells

parseBoard :: [[Int]] -> Board
parseBoard b =
  let (xs, rows, cols) =
        unzip3 $
          [ (((i, j), value), i, j)
          | (row, i) <- zip b [0 ..]
          , (value, j) <- zip row [0 ..]
          ]
   in Board (maximum rows + 1) (maximum cols + 1) (M.fromAscList xs)

getSize :: Reader Board (Int, Int)
getSize = asks $ \(Board h w _) -> (h, w)

getCells :: Reader Board Cells
getCells = asks $ \(Board _ _ cells) -> cells

neighbors :: Point -> Reader Board [Point]
neighbors (y, x) = do
  (h, w) <- getSize
  pure $
    [ (ny, nx)
    | dy <- [- 1 .. 1]
    , let ny = y + dy
    , 0 <= ny && ny < h
    , dx <- [- 1 .. 1]
    , let nx = x + dx
    , 0 <= nx && nx < w
    , not (dx == 0 && dy == 0)
    ]

getValue :: Point -> Reader Board Int
getValue p = fromMaybe 0 . M.lookup p <$> getCells

{-# ANN lowPoints ("HLint: ignore Redundant <$>" :: String) #-}
lowPoints :: Reader Board [Point]
lowPoints =
  (M.keys <$> getCells) >>= filterM \p -> do
    value <- getValue p
    nValues <- neighbors p >>= traverse getValue
    pure $ all (> value) nValues

solve1 :: Reader Board Int
solve1 = sum . map (+ 1) <$> (lowPoints >>= traverse getValue)

{- Part two -}

neighborsHV p@(y, x) = filter (\(ny, nx) -> ny == y || nx == x) <$> neighbors p

upwardNeighborsHV :: Point -> Reader Board [Point]
upwardNeighborsHV p = do
  value <- getValue p
  ns <- neighborsHV p >>= traverse (\n -> (n,) <$> getValue n)
  pure . map fst . filter ((\nv -> nv /= 9 && nv > value) . snd) $ ns

basinFrom :: Point -> Reader Board [Point]
basinFrom p = go [p] (S.singleton p)
 where
  go :: [Point] -> S.Set Point -> Reader Board [Point]
  go [] acc = pure . S.toList $ acc
  go (p : rest) acc = do
    next <- upwardNeighborsHV p
    let remaining = filter (not . (`S.member` acc)) (rest ++ next)
    go remaining (S.insert p acc)

solve2 :: Reader Board Int
solve2 =
  product . take 3 . reverse . L.sort . map length
    <$> (lowPoints >>= traverse basinFrom)

solutions =
  [ solve1
  , solve2
  ]

main :: IO ()
main = do
  idx <- read . head <$> getArgs
  print
    . runReader (solutions !! (idx - 1))
    . parseBoard
    . fmap (fmap C.digitToInt)
    . lines
    =<< getContents
