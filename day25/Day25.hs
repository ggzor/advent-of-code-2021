import Control.Applicative
import Data.Bifunctor
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe

import System.Environment

type Board = (Int, Int, M.Map (Int, Int) Char)

parse :: [String] -> Board
parse ls =
  ( length ls
  , length (head ls)
  , M.fromList
      [ ((y, x), v)
      | (y, r) <- zip [0 ..] ls
      , (x, v) <- zip [0 ..] r
      , v == 'v' || v == '>'
      ]
  )

nextCell '>' (_, w, _) = second (\x -> (x + 1) `mod` w)
nextCell 'v' (h, _, _) = first (\y -> (y + 1) `mod` h)

advance :: Char -> Board -> Maybe Board
advance c fb@(h, w, b) =
  let pick k v = c == v && '.' == fromMaybe '.' (M.lookup (nextCell c fb k) b)
      (target, other) = M.partitionWithKey pick b
      newTarget = M.fromList . map (first (nextCell c fb)) $ M.toList target
   in if M.null target
        then Nothing
        else Just (h, w, M.union other newTarget)

step b = (advance '>' b >>= advance 'v') <|> advance '>' b <|> advance 'v' b

solve b =
  length . takeWhile isJust
    . L.scanl' (flip (const (>>= step))) (Just b)
    $ repeat ()

main :: IO ()
main = print . solve . parse . lines =<< getContents
