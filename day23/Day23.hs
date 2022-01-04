{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}

import Control.Monad.Reader
import Data.Bifunctor
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S

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

{- monad-loops would be helpful here -}
spanM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
spanM _ [] = pure ([], [])
spanM p (x : xs) =
  p x >>= \case
    False -> pure ([], x : xs)
    True -> first (x :) <$> spanM p xs

takeWhileM :: Monad m => (a -> m Bool) -> [a] -> m [a]
takeWhileM p xs = fst <$> spanM p xs

allM p = foldM (\acc v -> (acc &&) <$> p v) True

{- Solution -}
type Point = (Int, Int)
type Movement = (Point, Point)
type Board = M.Map Point Char
type Diagram = (Int, Int, Board)

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

moveCosts = M.fromList [('A', 1), ('B', 10), ('C', 100), ('D', 1000)]
target = M.fromList [('A', 3), ('B', 5), ('C', 7), ('D', 9)]
targetCols = S.fromList $ M.elems target

type DiagramM = Reader Diagram

widthM :: DiagramM Int
widthM = asks $ \(_, w, _) -> w

boardM :: DiagramM Board
boardM = asks $ \(_, _, b) -> b

getOr def k m = fromMaybe def $ M.lookup k m
getAt p = getOr '.' p <$> boardM
isEmpty = fmap ('.' ==) . getAt

nextMovements :: DiagramM [Movement]
nextMovements =
  boardM >>= fmap concat . mapM (\p -> fmap (p,) <$> movementsForPoint p) . M.keys

movementsForPoint :: Point -> DiagramM [Point]
movementsForPoint p@(y, x) = do
  v <- getAt p
  (h, w, b) <- ask
  if
      | y == 1 -> filter (\(yi, _) -> yi /= 1) <$> bothSidesFrom p
      | y >= 2 -> do
        isRoomStartEmpty <- allM isEmpty $ map (,x) [y - 1, y - 2 .. 2]
        let mustMove = x /= target M.! v || any (\yi -> v /= b M.! (yi, x)) [y + 1 .. h - 2]
        if isRoomStartEmpty && mustMove then bothSidesFrom p else pure []

bothSidesFrom :: Point -> DiagramM [Point]
bothSidesFrom p@(_, x) = do
  w <- widthM
  l <- oneSide p [x - 1, x - 2 .. 1]
  r <- oneSide p [x + 1 .. w - 2]
  pure $ l ++ r

oneSide :: Point -> [Int] -> DiagramM [Point]
oneSide p sideIter = do
  v <- getAt p
  (h, w, b) <- ask
  hallway <- takeWhileM isEmpty $ map (1,) sideIter
  let (sideRoomTops, validHallway) =
        L.partition (\(_, x) -> S.member x targetCols) hallway
  sideRoomPoint <- case L.find (\(_, x) -> x == target M.! v) hallway of
    Nothing -> pure []
    Just (_, xi) -> do
      (start, end) <- spanM isEmpty $ map (,xi) [2 .. h - 2]
      pure [last start | all ((v ==) . (b M.!)) end]
  pure $ validHallway ++ sideRoomPoint

applyMovement :: Board -> Movement -> Board
applyMovement b (src, target) = M.insert target (b M.! src) $ M.delete src b

distance (y1, x1) (y2, x2) = abs (x1 - x2) + (y1 - 1) + (y2 - 1)

nextBoards :: Diagram -> [(Int, Board)]
nextBoards d@(_, _, b) =
  map
    ( \m@(src, target) ->
        ( distance src target * moveCosts M.! (b M.! src)
        , applyMovement b m
        )
    )
    $ runReader nextMovements d

scanBoards :: Diagram -> [(Int, Board)]
scanBoards (h, w, board) = go (qsingleton 0 board) S.empty
 where
  go :: PQueue Int Board -> S.Set Board -> [(Int, Board)]
  go q _ | qnull q = []
  go q seen =
    let (r@(dist, board), qnext) = deleteFindMin q
        new =
          if S.member board seen
            then []
            else
              map (first (+ dist)) . filter ((`S.notMember` seen) . snd) $
                nextBoards (h, w, board)
     in r : go (L.foldl' (\q (c, b) -> qinsert c b q) qnext new) (S.insert board seen)

isGoal = all (\((y, x), v) -> y >= 2 && x == target M.! v) . M.toList

solve :: Diagram -> Int
solve = maybe 0 fst . L.find (isGoal . snd) . scanBoards

main :: IO ()
main = print . solve . parseBoard . lines =<< getContents
