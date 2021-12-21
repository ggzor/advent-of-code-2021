{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

import Data.Bifunctor
import Data.Either
import qualified Data.List as L
import qualified Data.Map.Strict as M

import System.Environment

-- Just a primitive lens
data Turn = Turn {_get :: (Int, Int) -> Int, _set :: Int -> (Int, Int) -> (Int, Int)}
data Game = Game
  { _positions :: {-# UNPACK #-} !(Int, Int)
  , _scores :: {-# UNPACK #-} !(Int, Int)
  }
  deriving (Eq, Ord, Show)

type DiceRolls = [[[Int]]]

stdTurns = [Turn fst (first . const), Turn snd (second . const)]

adjust base value amount = (value - 1 + amount) `mod` base + 1

step :: Turn -> Game -> Int -> Game
step (Turn get set) (Game positions scores) die =
  let p = adjust 10 (get positions) die
      s = get scores + p
   in Game (set p positions) (set s scores)

untilScoreReaches :: Int -> DiceRolls -> Game -> (Int, [(Game, Int)])
untilScoreReaches amount rolls start =
  go 0 rolls (cycle stdTurns) (M.singleton start 1) []
 where
  go n _ _ universes acc | M.null universes = (n, acc)
  go n (r : rs) (t : ts) universes acc =
    let (finished, remaining) =
          L.partition (\(Game ps (s1, s2), _) -> s1 >= amount || s2 >= amount)
            . concatMap (\(g, c) -> map ((,c) . step t g . sum) $ sequence r)
            $ M.toList universes
        newUniverses = L.foldl' (\m (g, c) -> M.insertWith (+) g c m) M.empty remaining
     in go (n + length r) rs ts newUniverses (finished ++ acc)

solveGen :: DiceRolls -> Int -> (Int, Int) -> (Int, [(Game, Int)])
solveGen rolls limit ps = untilScoreReaches limit rolls (Game ps (0, 0))

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l = let (g, rest) = L.splitAt n l in g : chunksOf n rest

-- [[[1], [2], [3]], [[4], [5], [6]], ...]
die100 = chunksOf 3 . map (: []) . cycle $ [1 .. 100]
solve1 = (\(rolls, [(Game _ (s1, s2), _)]) -> rolls * min s1 s2) . solveGen die100 1000

{- Part two -}

-- [[[1, 2, 3], [1, 2, 3], [1, 2, 3]], ...]
dieDirac = chunksOf 3 . repeat $ [1, 2, 3]
solve2 =
  uncurry max
    . bimap sum sum
    . partitionEithers
    . map (\(Game _ (s1, s2), c) -> if s1 > s2 then Left c else Right c)
    . snd
    . solveGen dieDirac 21

solutions =
  [ solve1
  , solve2
  ]

main :: IO ()
main = do
  idx <- read . head <$> getArgs
  print
    . (solutions !! (idx - 1))
    . (\[a, b] -> (a, b))
    . fmap (read @Int . last . words)
    . lines
    =<< getContents
