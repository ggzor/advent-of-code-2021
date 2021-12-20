{-# LANGUAGE TupleSections #-}

import Data.Char as C
import Data.Function
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S

import System.Environment

data Vec3 = Vec3 !Int !Int !Int deriving (Show, Eq, Ord)

permute (Vec3 x y z) =
  [ Vec3 x y z
  , Vec3 (- z) y x
  , Vec3 (- x) y (- z)
  , Vec3 z y (- x)
  , Vec3 (- x) (- y) z
  , Vec3 (- z) (- y) (- x)
  , Vec3 x (- y) (- z)
  , Vec3 z (- y) x
  , Vec3 (- y) x z
  , Vec3 (- y) (- z) x
  , Vec3 (- y) (- x) (- z)
  , Vec3 (- y) z (- x)
  , Vec3 y (- x) z
  , Vec3 y (- z) (- x)
  , Vec3 y x (- z)
  , Vec3 y z x
  , Vec3 x (- z) y
  , Vec3 (- z) (- x) y
  , Vec3 (- x) z y
  , Vec3 z x y
  , Vec3 x z (- y)
  , Vec3 (- z) x (- y)
  , Vec3 (- x) (- z) (- y)
  , Vec3 z (- x) (- y)
  ]

componentWise f (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (f x1 x2) (f y1 y2) (f z1 z2)
(-.) = componentWise (-)
(+.) = componentWise (+)

type ScannerData = S.Set Vec3

parse :: [String] -> [ScannerData]
parse =
  map
    ( S.fromList
        . map
          ( (\[x, y, z] -> Vec3 (read x) (read y) (read z))
              . filter (notElem ',')
              . L.groupBy ((==) `on` (== ','))
          )
        . tail
    )
    . filter (not . any null)
    . L.groupBy ((==) `on` null)

type OverlapResult = (ScannerData, Vec3)

headOption :: [a] -> Maybe a
headOption [] = Nothing
headOption (x : _) = Just x

checkOverlap :: ScannerData -> ScannerData -> Maybe OverlapResult
checkOverlap base target =
  let alternatives =
        [ (S.fromList $ map (-. diff) perm, diff)
        | bv <- S.toList base
        , perm <- L.transpose . map permute . S.toList $ target
        , tv <- perm
        , let diff = tv -. bv
        ]
   in headOption [t | t@(s, d) <- alternatives, S.size (base `S.intersection` s) >= 12]

findNext :: [ScannerData] -> [ScannerData] -> (OverlapResult, [ScannerData])
findNext base remaining = head . catMaybes $ []

relativize :: [ScannerData] -> [(ScannerData, Vec3)]
relativize (sd : rest) =
  go (M.singleton 0 (sd, Vec3 0 0 0)) [0] (M.fromList $ zip [1 ..] rest)
 where
  go base _ remaining | M.null remaining = M.elems base
  go base (n : rest) remaining =
    let (next, _) = base M.! n
        newOnes =
          mapMaybe (\(i, other) -> (i,) <$> checkOverlap next other) $
            M.toList remaining
        newIdx = map fst newOnes
        newBase = L.foldl' (flip $ uncurry M.insert) base newOnes
        newRemaining = L.foldl' (flip M.delete) remaining newIdx
     in go newBase (rest ++ newIdx) newRemaining

solve1 :: [ScannerData] -> Int
solve1 = S.size . S.unions . map fst . relativize

solve2 :: [ScannerData] -> Int
solve2 = pairwiseMaxMan . map snd . relativize
 where
  pairwiseMaxMan l =
    maximum
      . zipWith (\d1 -> maximum . map (\d2 -> absSum $ d1 -. d2)) l
      $ (init . tail $ L.tails l)
  absSum (Vec3 x y z) = abs x + abs y + abs z

solutions =
  [ solve1
  , solve2
  ]

main :: IO ()
main = do
  idx <- read . head <$> getArgs
  print
    . (solutions !! (idx - 1))
    . parse
    . lines
    =<< getContents
