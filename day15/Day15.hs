import Data.Char as C
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Tuple

import System.Environment

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

{- Part one -}
type Point = (Int, Int)
type PointMap = M.Map Point Int
type Board = ((Int, Int), PointMap)

parseBoard :: [[Char]] -> Board
parseBoard b =
  let board =
        M.fromList $
          [((y, x), C.digitToInt v) | (r, y) <- zip b [0 ..], (v, x) <- zip r [0 ..]]
      boundBy f = (+ 1) . maximum . map f $ M.keys board
   in ((boundBy fst, boundBy snd), board)

solve1 :: Board -> Int
solve1 ((h, w), b) =
  go
    S.empty
    (M.insert (0, 0) 0 $ M.map (const maxBound) b)
    (qsingleton 0 (0, 0))
    M.! (h -1, w -1)
 where
  go :: S.Set Point -> PointMap -> PQueue Int Point -> PointMap
  go seen ds q =
    let ((len, cur@(y, x)), qrest) = deleteFindMin q
        neighbors =
          [ p
          | (dy, dx) <- [(-1, 0), (0, -1), (0, 1), (1, 0)]
          , let p@(ny, nx) = (y + dy, x + dx)
          , 0 <= ny && ny < h && 0 <= nx && nx < w
          , not $ p `S.member` seen
          ]
        old = (ds M.!) <$> neighbors
        new = (len +) . (b M.!) <$> neighbors
        updated = [(p, nd) | (p, od, nd) <- zip3 neighbors old new, nd < od]
     in if qnull q
          then ds
          else
            go
              (S.insert cur seen)
              (L.foldl' (flip . uncurry $ M.insert) ds updated)
              (L.foldl' (flip . uncurry $ qinsert) qrest (map swap updated))

{- Part two -}
alterBoard :: Board -> Board
alterBoard (size@(h, w), b) =
  ( (h * 5, w * 5)
  , M.fromList
      [ ((y + sy * h, x + sx * w), if nv <= 9 then nv else nv - 9)
      | (p@(y, x), v) <- M.toList b
      , sy <- [0 .. 4]
      , sx <- [0 .. 4]
      , let nv = v + sy + sx
      ]
  )

solve2 = solve1 . alterBoard

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
