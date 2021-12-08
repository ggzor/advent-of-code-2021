{-# LANGUAGE TupleSections #-}

import Data.Function
import Data.Functor
import qualified Data.List as L
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Data.Tuple

import System.Environment

type Segment = S.Set Char

isRelevantChar = (`S.member` S.fromList "abcdefg|")

parseSegments :: String -> (S.Set Segment, [Segment])
parseSegments s = (S.fromList digits, target)
 where
  [digits, _, target] =
    L.groupBy ((==) `on` ('|' `S.member`))
      . map S.fromList
      . filter (isRelevantChar . head)
      . L.groupBy ((==) `on` isRelevantChar)
      $ s

solve1 :: [(S.Set Segment, [Segment])] -> Int
solve1 = length . filter ((`S.member` S.fromList [2, 3, 4, 7]) . S.size) . concatMap snd

{- Part two -}

determineDigits :: S.Set Segment -> M.Map Segment Char
determineDigits s =
  let lenToDigit len = case len of
        2 -> Just '1'
        3 -> Just '7'
        4 -> Just '4'
        7 -> Just '8'
        _ -> Nothing
      knownByLen = M.fromList . mapMaybe (\s -> (,s) <$> lenToDigit (S.size s)) $ S.elems s
      [segs_adg, segs_abfg] =
        [5, 6] <&> \size ->
          L.foldl1' S.intersection
            . filter ((size ==) . S.size)
            $ S.elems s
      seg_d = segs_adg `S.difference` segs_abfg

      digit_5 = segs_abfg `S.union` seg_d
      seg_c = (knownByLen ! '1') `S.difference` digit_5
      digit_9 = digit_5 `S.union` seg_c
      seg_e = (knownByLen ! '8') `S.difference` digit_9
      digit_6 = digit_5 `S.union` seg_e

      digit_0 = (knownByLen ! '8') `S.difference` seg_d

      digit_3 = segs_adg `S.union` (knownByLen ! '7')
      digit_2 = segs_adg `S.union` ((knownByLen ! '8') `S.difference` digit_5)
   in M.fromList . map swap $
        [ ('0', digit_0)
        , ('2', digit_2)
        , ('3', digit_3)
        , ('5', digit_5)
        , ('6', digit_6)
        , ('9', digit_9)
        ]
          ++ M.toList knownByLen

solveLine :: (S.Set Segment, [Segment]) -> Int
solveLine (digits, target) =
  maybe 0 read . traverse (`M.lookup` determineDigits digits) $ target

solve2 = sum . map solveLine

solutions =
  [ solve1
  , solve2
  ]

main :: IO ()
main = do
  idx <- read . head <$> getArgs
  print
    . (solutions !! (idx - 1))
    . fmap parseSegments
    . lines
    =<< getContents
