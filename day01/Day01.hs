{-# LANGUAGE TypeApplications #-}

import System.Environment (getArgs)

solve1 :: [Int] -> Int
solve1 l = length . filter id $ zipWith (<) l (tail l)

solve2 :: [Int] -> Int
solve2 l = solve1 $ zipWith3 (\a b c -> a + b + c) l (tail l) (tail (tail l))

solutions = [solve1, solve2]

main :: IO ()
main = do
  idx <- read . head <$> getArgs
  print
    . (solutions !! (idx - 1))
    . fmap (read @Int)
    . lines
    =<< getContents
