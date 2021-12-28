{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

import Data.Char as C
import Data.Function
import qualified Data.List as L
import Data.Maybe

import System.Environment

data Range = Range {_start :: !Int, _end :: !Int} deriving (Show)
mkRange s e | s <= e = Just $ Range s e
mkRange _ _ = Nothing

intersect r1 r2 =
  let (rl@(Range s1 e1), Range s2 e2) = if _end r1 > _end r2 then (r2, r1) else (r1, r2)
   in if
          | e1 < s2 -> Nothing
          | s1 >= s2 -> Just rl
          | otherwise -> Just $ Range (max s1 s2) (min e1 e2)

rsize (Range s e) = e - s + 1

data Box = Box {_xs :: !Range, _ys :: !Range, _zs :: !Range} deriving (Show)

(&.) :: Box -> Box -> Maybe Box
Box x1 y1 z1 &. Box x2 y2 z2 =
  Box <$> intersect x1 x2 <*> intersect y1 y2 <*> intersect z1 z2

-- Imagine b2 as a cube inside b1
(-.) :: Box -> Box -> [Box]
(-.)
  b1@(Box x1@(Range xs1 xe1) y1@(Range ys1 ye1) z1@(Range zs1 ze1))
  b2@(Box x2@(Range xs2 xe2) y2@(Range ys2 ye2) z2@(Range zs2 ze2)) =
    fromMaybe [b1] $
      (b1 &. b2)
        *> Just
          ( catMaybes
              [ Box <$> mkRange xs1 (xs2 - 1) <*> intersect y1 y2 <*> intersect z1 z2
              , Box <$> mkRange (xe2 + 1) xe1 <*> intersect y1 y2 <*> intersect z1 z2
              , Box x1 <$> mkRange ys1 (ys2 - 1) <*> intersect z1 z2
              , Box x1 <$> mkRange (ye2 + 1) ye1 <*> intersect z1 z2
              , Box x1 y1 <$> mkRange zs1 (zs2 - 1)
              , Box x1 y1 <$> mkRange (ze2 + 1) ze1
              ]
          )

bsize (Box xs ys zs) = rsize xs * rsize ys * rsize zs

isCharForNumber = (||) <$> (== '-') <*> C.isNumber
numbers =
  map (read @Int)
    . filter (isCharForNumber . head)
    . L.groupBy ((==) `on` isCharForNumber)

type Instruction = (Bool, Box)

parseInstruction :: String -> Maybe Instruction
parseInstruction s =
  let [x1, x2, y1, y2, z1, z2] = numbers s
      isOn = ("on" ==) . head $ words s
   in (isOn,) <$> (Box <$> mkRange x1 x2 <*> mkRange y1 y2 <*> mkRange z1 z2)

ir = Range (-50) 50

solve1 :: [Instruction] -> Int
solve1 = solve2 . mapMaybe (\(on, box) -> (on,) <$> (box &. Box ir ir ir))

solve2 :: [Instruction] -> Int
solve2 insts =
  let apply :: [Box] -> Instruction -> [Box]
      apply boxes (True, newBox) =
        boxes ++ L.foldl' (\acc old -> concatMap (-. old) acc) [newBox] boxes
      apply boxes (False, newBox) = concatMap (-. newBox) boxes
   in sum . map bsize . L.foldl' apply [] $ insts

solutions =
  [ solve1
  , solve2
  ]

main :: IO ()
main = do
  idx <- read . head <$> getArgs
  print
    . (solutions !! (idx - 1))
    . mapMaybe parseInstruction
    . lines
    =<< getContents
