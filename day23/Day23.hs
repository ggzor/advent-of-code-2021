{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

import Control.Applicative
import Control.Monad
import Data.Bifunctor
import Data.Char as C
import Data.Function
import Data.Functor
import qualified Data.IntMap.Strict as IM
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Ord
import qualified Data.Set as S
import Data.Tuple
import Numeric

import Control.Monad.Reader
import System.Environment
import System.IO.Unsafe (unsafePerformIO)

debug a = seq (unsafePerformIO $ print a)

compDebug :: Show a => a -> a
compDebug = debug <$> id <*> id

{- Minimal priority queue implementation -}
newtype PQueue k a = PQueue (M.Map k [a])
deleteFindMin :: Ord k => PQueue k a -> ((k, a), PQueue k a)
deleteFindMin (PQueue m) =
  let ((k, a : other), mrest) = M.deleteFindMin m
      newM = if null other then mrest else M.insert k other mrest
   in ((k, a), PQueue newM)

qsingleton k a = PQueue $ M.singleton k [a]
qnull (PQueue m) = M.null m
qinsert k a (PQueue m) = PQueue $ M.insertWith (++) k [a] m

{- Solution -}

temph = 5
tempw = 13

isSpaceB y x = y >= 3 && (x <= 1 || x >= tempw - 2)
isBorderB y x =
  y == 0 || y == 1 && (x == 0 || x == tempw - 1)
    || y >= 2 && notElem x (M.elems target)
    || y == temph - 1

pprintBoard b =
  unlines
    [ [ if
          | isSpaceB y x -> ' '
          | isBorderB y x -> '#'
          | otherwise -> fromMaybe '.' $ M.lookup (y, x) b
      | x <- [0 .. tempw - 1]
      ]
    | y <- [0 .. temph - 1]
    ]

compShowBoard :: (Int, Board) -> (Int, Board)
compShowBoard b@(cost, board) =
  let action = do
        print cost
        putStrLn . pprintBoard $ board
   in seq (unsafePerformIO action) b

type Point = (Int, Int)
type Movement = (Point, Point)
type Board = M.Map Point Char
type Diagram = (Int, Int, Board)

moveCosts = M.fromList [('A', 1), ('B', 10), ('C', 100), ('D', 1000)]
target = M.fromList [('A', 3), ('B', 5), ('C', 7), ('D', 9)]
targetCols = S.fromList $ M.elems target

parseBoard :: [String] -> Diagram
parseBoard ls =
  ( length ls
  , length (head ls)
  , M.fromList
      [ ((y, x), c)
      | (y, r) <- zip [0 ..] ls
      , (x, c) <- zip [0 ..] r
      , c `M.member` moveCosts
      ]
  )

isGoal = all (\((y, x), v) -> y >= 2 && x == target M.! v) . M.toList

getOr def k m = fromMaybe def $ M.lookup k m

nextMovements :: Diagram -> [(Int, Movement)]
nextMovements d@(h, w, board) = concatMap checkPoint $ M.toList board
 where
  checkPoint :: (Point, Char) -> [(Int, Movement)]
  checkPoint p@((y, x), v) | y == 1 = filter (\(_, (_, (yi, _))) -> yi /= 1) $ bothSides d p
  checkPoint p@((y, x), v)
    | y >= 2
        && all (\yi -> '.' == getOr '.' (yi, x) board) [y - 1, y - 2 .. 2]
        && ( x /= target M.! v
              || any (\yi -> v /= board M.! (yi, x)) [y + 1 .. h - 2]
           ) =
      map (first (+ (y - 1))) $ bothSides d p
  checkPoint p = []

nextBoards :: Diagram -> [(Int, Board)]
nextBoards d@(_, _, b) =
  map (second (applyMovement b) . (\(c, m@(psrc, ptarget)) -> (c * moveCosts M.! (b M.! psrc), m))) $ nextMovements d

applyMovement :: Board -> Movement -> Board
applyMovement b (psrc, ptarget) = M.insert ptarget (b M.! psrc) $ M.delete psrc b

bothSides :: Diagram -> (Point, Char) -> [(Int, Movement)]
bothSides b@(_, w, _) p@((_, x), _) =
  oneSide b p [x - 1, x - 2 .. 1] ++ oneSide b p [x + 1 .. w - 2]

oneSide :: Diagram -> (Point, Char) -> [Int] -> [(Int, Movement)]
oneSide d@(h, w, b) (psrc@(y, x), v) it =
  let hallwayPoints = takeWhile (\p -> '.' == getOr '.' p b) . map (1,) $ it
      (sideRoomTops, validHallway) = L.partition ((`S.member` targetCols) . snd) hallwayPoints
      hallwayTargets = map (\p@(yi, xi) -> (abs $ x - xi, (psrc, p))) validHallway
      sidePoint = L.find ((== target M.! v) . snd) sideRoomTops
      sideRoomTarget = case sidePoint of
        Nothing -> []
        Just (_, xi) ->
          let (initial, final) = L.span (\p -> '.' == getOr '.' p b) . map (,xi) $ [2 .. h - 2]
              sideRoomPoint = [last initial | all (\p -> v == b M.! p) final]
           in map (\p@(yi, xi) -> ((yi - 1) + abs (x - xi), (psrc, p))) sideRoomPoint
   in hallwayTargets ++ sideRoomTarget

scanBoards :: Diagram -> [(Int, Board)]
scanBoards (h, w, board) = go (qsingleton 0 board) S.empty
 where
  go :: PQueue Int Board -> S.Set Board -> [(Int, Board)]
  go q _ | qnull q = []
  go q seen =
    let (t@(dist, board), qnext) = deleteFindMin q
        next =
          if S.member board seen
            then []
            else map (first (+ dist)) . filter ((`S.notMember` seen) . snd) $ nextBoards (h, w, board)
     in t : go (L.foldl' (\q (c, b) -> qinsert c b q) qnext next) (S.insert board seen)

solve :: Diagram -> Int
solve = maybe 0 fst . L.find (isGoal . snd) . scanBoards

main :: IO ()
main = print . solve . parseBoard . lines =<< getContents
