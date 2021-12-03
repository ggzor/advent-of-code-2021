import Data.List (foldl')

import Control.Monad.State.Strict
import Data.Foldable (traverse_)
import System.Environment

type Inst = (String, Int)

parseInst s =
  let [dir, num] = words s
   in (dir, read num)

mapInst ("forward", dx) = (dx, 0)
mapInst ("down", dy) = (0, dy)
mapInst ("up", dy) = (0, - dy)

sumTuple (a, b) (c, d) = (a + c, b + d)
prodTuple (x, y) = x * y

solve1 :: [Inst] -> Int
solve1 = prodTuple . foldl' sumTuple (0, 0) . map mapInst

{- Part 2 -}

evalInst :: Inst -> State (Int, Int, Int) ()
evalInst dir = modify $ \(x, y, aim) ->
  case dir of
    ("down", da) -> (x, y, aim + da)
    ("up", da) -> (x, y, aim - da)
    ("forward", dx) -> (x + dx, y + aim * dx, aim)

prodTupleFirst2 (x, y, z) = x * y

solve2 :: [Inst] -> Int
solve2 = prodTupleFirst2 . flip execState (0, 0, 0) . traverse_ evalInst

solutions =
  [ solve1
  , solve2
  ]

main :: IO ()
main = do
  idx <- read . head <$> getArgs
  print
    . (solutions !! (idx - 1))
    . fmap parseInst
    . lines
    =<< getContents
