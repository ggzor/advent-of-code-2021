import Data.Function
import qualified Data.List as L
import qualified Data.Map.Strict as M

import System.Environment

type Pair = (Char, Char)
type Rules = M.Map Pair Char
type Input = (M.Map Pair Int, M.Map Char Int, Rules)

count :: Ord a => [a] -> M.Map a Int
count = L.foldl' (\m k -> M.insertWith (+) k 1 m) M.empty

increase :: Ord a => a -> Int -> M.Map a Int -> M.Map a Int
increase = M.insertWith (+)

parseInput :: [String] -> Input
parseInput (temp : _ : rules) =
  ( count $ zip temp (tail temp)
  , count temp
  , M.fromList . map (\(c1 : c2 : rest) -> ((c1, c2), last rest)) $ rules
  )

step :: Input -> Input
step (pairs, chars, rules) =
  let (ps, cs) = M.foldlWithKey' processPair (M.empty, chars) pairs
      processPair (ps, cs) p@(p1, p2) v =
        let mid = rules M.! p
         in ( increase (p1, mid) v . increase (mid, p2) v $ ps
            , increase mid v cs
            )
   in (ps, cs, rules)

steps start = L.scanl' (&) start $ repeat step

solveGen :: Input -> [Int]
solveGen = map (\(_, cs, _) -> ((-) <$> maximum <*> minimum) $ M.elems cs) . steps

solve1 = (!! 10) . solveGen
solve2 = (!! 40) . solveGen

solutions = [solve1, solve2]

main :: IO ()
main = do
  idx <- read . head <$> getArgs
  print
    . (solutions !! (idx - 1))
    . parseInput
    . lines
    =<< getContents
