{-# LANGUAGE TypeApplications #-}

import Control.Monad.State.Strict
import Data.Char as C
import Data.Function
import qualified Data.List as L

import System.Environment

data SimState = SimState
  { _position :: !(Int, Int)
  , _velocity :: !(Int, Int)
  , _maxY :: !Int
  , _hit :: !Bool
  }

type Area = (Int, Int, Int, Int)

simulate :: Area -> State SimState ()
simulate area@(xmin, xmax, ymin, ymax) = do
  (x, y) <- gets _position
  (xv, yv) <- gets _velocity
  maxY <- gets _maxY
  hit <- gets _hit
  put $
    SimState
      { _position = (x + xv, y + yv)
      , _velocity = (xv - signum xv, yv - 1)
      , _maxY = max maxY (y + yv)
      , _hit = hit || xmin <= x && x <= xmax && ymin <= y && y <= ymax
      }
  if y < ymin || x > xmax || (xv == 0 && x < xmin)
    then pure ()
    else simulate area

simulateRange :: [Int] -> [Int] -> Area -> (Int, Int)
simulateRange xrange yrange area =
  let results =
        [ (_maxY final, _hit final)
        | xv <- xrange
        , yv <- yrange
        , let final =
                execState
                  (simulate area)
                  SimState
                    { _position = (0, 0)
                    , _velocity = (xv, yv)
                    , _maxY = 0
                    , _hit = False
                    }
        , _hit final
        ]
   in (,) <$> maximum . map fst <*> length $ results

guess = 300
solveGen a@(xmin, xmax, ymin, ymax) =
  simulateRange [1 .. xmax + 1] [ymin - 1 .. guess] a

solve1 = fst . solveGen
solve2 = snd . solveGen

solutions = [solve1, solve2]

isIntChar = (||) <$> (== '-') <*> C.isNumber
numbers =
  map (read @Int)
    . filter (isIntChar . head)
    . L.groupBy ((==) `on` isIntChar)

main :: IO ()
main = do
  idx <- read . head <$> getArgs
  print
    . (solutions !! (idx - 1))
    . (\[a, b, c, d] -> (a, b, c, d))
    . numbers
    =<< getContents
