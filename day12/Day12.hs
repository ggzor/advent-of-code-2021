{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}

import Data.Bifunctor
import Data.Char as C
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import System.Environment

type Node = String
type Path = [Node]
type Graph = M.Map (Node, Node) ()

undirected (x, y) = [(x, y), (y, x)]
parseGraph =
  M.fromList
    . concatMap
      ( map (,()) . undirected
          . second tail
          . span (/= '-')
      )

neighbors n = map snd . filter ((n ==) . fst) . M.keys

isStart = (== "start")
isEnd = (== "end")
isCave s = all ($ s) [not . isStart, not . isEnd]
isSmall s = all ($ s) [isCave, all C.isLower]
isBig s = all ($ s) [isCave, all C.isUpper]

dfs :: Node -> S.Set Node -> Graph -> Int
dfs cur seen g =
  if
      | cur `S.member` seen -> 0
      | isEnd cur -> 1
      | otherwise ->
        let newSeen =
              if not . isBig $ cur
                then S.insert cur seen
                else seen
         in sum . map (\n -> dfs n newSeen g) $ neighbors cur g

solve1 :: Graph -> Int
solve1 = dfs "start" S.empty

{- Part two -}

dfsTwo :: Node -> S.Set Node -> M.Map Node Int -> Graph -> Int
dfsTwo cur seen counts g =
  if
      | (> 1) . length . filter (== 2) $ M.elems counts -> 0
      | S.member cur seen -> 0
      | (Just 2 ==) $ M.lookup cur counts -> 0
      | isEnd cur -> 1
      | otherwise ->
        let newSeen = if not (isCave cur) then S.insert cur seen else seen
            newCounts = if isSmall cur then M.insertWith (+) cur 1 counts else counts
         in sum . map (\n -> dfsTwo n newSeen newCounts g) $ neighbors cur g

solve2 :: Graph -> Int
solve2 = dfsTwo "start" S.empty M.empty

solutions =
  [ solve1
  , solve2
  ]

main :: IO ()
main = do
  idx <- read . head <$> getArgs
  print
    . (solutions !! (idx - 1))
    . parseGraph
    . lines
    =<< getContents
