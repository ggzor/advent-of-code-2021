import Data.Bifunctor
import qualified Data.List as L
import qualified Data.Set as S

import System.Environment

type Paper = ((Int, Int), S.Set (Int, Int))
type Fold = (Char, Int)
type Input = (Paper, [Fold])

parseInput :: [String] -> Input
parseInput ls =
  let (ps, _ : fs) = break null ls
      ns = map (bimap read (read . tail) . break (== ',')) ps
      ifs = map (bimap last (read . tail) . break (== '=')) fs
      p = S.fromList ns
      boundOf c = maximum (map snd . filter ((c ==) . fst) $ ifs) * 2 + 1
   in (((boundOf 'x', boundOf 'y'), p), ifs)

foldPaper :: Fold -> Paper -> Paper
foldPaper (dir, n) ((w, h), paper) =
  let foldPoint (px, py) = case dir of
        'x' | px >= n -> (w - px - 1, py)
        'y' | py >= n -> (px, h - py - 1)
        _ -> (px, py)
      updateSize = if dir == 'x' then first else second
   in (updateSize (`div` 2) (w, h), S.map foldPoint paper)

solveGen :: Input -> [Paper]
solveGen = uncurry $ L.scanl' (flip foldPaper)

solve1 = print . S.size . snd . (!! 1) . solveGen
solve2 inp =
  let ((w, h), p) = last . solveGen $ inp
      board =
        [ [if (x, y) `S.member` p then '#' else ' ' | x <- [0 .. w - 1]]
        | y <- [0 .. h - 1]
        ]
   in putStr $ unlines board

solutions =
  [ solve1
  , solve2
  ]

main :: IO ()
main = do
  idx <- read . head <$> getArgs
  (solutions !! (idx - 1))
    . parseInput
    . lines
    =<< getContents
