import Data.Function
import Data.Functor
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe

import System.Environment

type Algorithm = M.Map Int Bool
type Image = M.Map (Int, Int) Bool
type Input = (Algorithm, Image)

parse :: [String] -> Input
parse (code : _ : img) =
  ( M.fromList . zip [0 ..] . map (== '#') $ code
  , M.fromList $ [((y, x), v == '#') | (y, row) <- zip [0 ..] img, (x, v) <- zip [0 ..] row]
  )

boolsToInt :: [Bool] -> Int
boolsToInt = L.foldl' (\a b -> a * 2 + if b then 1 else 0) 0

step :: Algorithm -> Int -> Image -> Image
step algo n img =
  let empty =
        if even n
          then algo M.! 0
          else algo M.! boolsToInt (replicate 9 (algo M.! 0))
      [minY, maxY, minX, maxX] =
        [(minimum, fst), (maximum, fst), (minimum, snd), (maximum, snd)] <&> \(f, g) ->
          f . map g . M.keys $ img
   in M.fromList
        [ ((y, x), nv)
        | y <- [minY - 1 .. maxY + 1]
        , x <- [minX - 1 .. maxX + 1]
        , let nv =
                (algo M.!) . boolsToInt $
                  [ fromMaybe empty $ M.lookup (y + dy, x + dx) img
                  | dy <- [-1 .. 1]
                  , dx <- [-1 .. 1]
                  ]
        ]

solveGen :: Input -> [Image]
solveGen (algorithm, img) = L.scanl' (&) img . map (step algorithm) $ [1 ..]

solve1 = length . filter id . M.elems . (!! 2) . solveGen
solve2 = length . filter id . M.elems . (!! 50) . solveGen

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
