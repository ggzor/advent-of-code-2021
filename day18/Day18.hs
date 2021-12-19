{-# LANGUAGE TupleSections #-}

import Control.Applicative
import Data.Bifunctor
import Data.Char as C
import Data.Function
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Data.Tuple

import System.Environment

-- `Data.Sequence` is the best data structure for this problem, but I preferred
-- to "keep it simple" by using Lists. Unfortunately, this leads to an abuse of
-- `reverse` and weird reversed checks.
type Path = String
type Leaf = (Int, Path)
type FlattenedTree = [Leaf]

tokenize =
  concatMap (\l@(c : _) -> if C.isNumber c then [l] else map (: []) l)
    . L.groupBy ((==) `on` C.isNumber)

parseTree :: [String] -> FlattenedTree
parseTree = map (second reverse) . reverse . snd . L.foldl' go ("", [])
 where
  go (p, acc) "[" = ('L' : p, acc)
  go (_ : p, acc) "," = ('R' : p, acc)
  go (_ : p, acc) "]" = (p, acc)
  go (p, acc) num@(c : _) | C.isNumber c = (p, (read num, p) : acc)

addTree a b = map (second ('L' :)) a ++ map (second ('R' :)) b

explode :: FlattenedTree -> Maybe FlattenedTree
explode t =
  let (result, changed) =
        bimap (reverse . catMaybes) getAny
          . L.foldl' nextLeaf ([], Any False)
          . map (,Any False)
          $ (Nothing : map Just t ++ [Nothing])
      nextLeaf (Just (bv, bp) : Just (av, ap) : l : rest, Any False) (r, _)
        | length ap == 5 && init ap == init bp =
          ([first (+ bv) <$> r, Just (0, init ap), first (+ av) <$> l] ++ rest, Any True)
      nextLeaf a b = first (: []) b <> a
   in if changed then Just result else Nothing

split :: FlattenedTree -> Maybe FlattenedTree
split t =
  let (result, changed) =
        bimap reverse getAny
          . L.foldl' nextLeaf ([], Any False)
          . map (,Any False)
          $ t
      nextLeaf (rest, Any False) ((v, p), _)
        | v >= 10 =
          ([(v - v `div` 2, p ++ "R"), (v `div` 2, p ++ "L")] ++ rest, Any True)
      nextLeaf a b = first (: []) b <> a
   in if changed then Just result else Nothing

explodeOrSplit = (<|>) <$> explode <*> split

reduceTree :: FlattenedTree -> FlattenedTree
reduceTree t =
  last . catMaybes . takeWhile isJust . L.scanl' (&) (Just t) $
    repeat (explodeOrSplit =<<)

magnitude :: FlattenedTree -> Int
magnitude t =
  let idx = M.fromList . map swap $ t
      go p = M.lookup p idx & fromMaybe (3 * go (p ++ "L") + 2 * go (p ++ "R"))
   in go ""

solve1 = magnitude . L.foldl1' (\a b -> reduceTree (addTree a b))
solve2 ts =
  maximum . concat
    . zipWith
      (\t1 -> concatMap (\t2 -> solve1 <$> [[t1, t2], [t2, t1]]))
      ts
    $ (tail . L.tails $ ts)

solutions =
  [ solve1
  , solve2
  ]

main :: IO ()
main = do
  idx <- read . head <$> getArgs
  print
    . (solutions !! (idx - 1))
    . fmap (parseTree . tokenize)
    . lines
    =<< getContents
