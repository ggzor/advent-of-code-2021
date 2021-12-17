{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

import Control.Applicative
import Control.Monad
import Data.Bifunctor
import Data.Char as C
import qualified Data.List as L
import Data.Maybe
import Numeric
import System.Environment

{- Minimal parser implementation -}
newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}
instance (Functor Parser) where
  fmap f (Parser p) = Parser $ fmap (first f) . p
instance (Applicative Parser) where
  pure v = Parser $ Just . (v,)
  (Parser pf) <*> (Parser pa) = Parser $ \s -> do
    (f, s2) <- pf s
    (a, s3) <- pa s2
    pure (f a, s3)
instance (Alternative Parser) where
  empty = Parser $ const Nothing
  (Parser pa) <|> (Parser pb) = Parser $ (<|>) <$> pa <*> pb
instance (Monad Parser) where
  (Parser pa) >>= f = Parser $ \s -> do
    (a, s2) <- pa s
    runParser (f a) s2

{- Parsing -}
char :: Char -> Parser Char
char c = Parser $ \case
  (x : xs) | c == x -> Just (c, xs)
  _ -> Nothing

binDigit = char '0' <|> char '1'
parseBin = L.foldl' (\a b -> a * 2 + C.digitToInt b) 0

type Version = Int
type PType = Int
data Payload = Literal Int | Operator [Packet] deriving (Show)
data Packet = Packet Version PType Payload deriving (Show)

pPacket :: Parser Packet
pPacket = do
  version <- pVersion
  ty <- pPType
  Packet version ty
    <$> if ty == 4
      then pLiteral
      else do
        lenTy <- binDigit
        if lenTy == '0'
          then do
            nbits <- parseBin <$> replicateM 15 binDigit
            packetData <- replicateM nbits binDigit
            let Just (subpackets, _) = runParser (many pPacket) packetData
            pure . Operator $ subpackets
          else do
            npackets <- parseBin <$> replicateM 11 binDigit
            Operator <$> replicateM npackets pPacket

pVersion = parseBin <$> replicateM 3 binDigit
pPType = pVersion

pLiteral = Literal . parseBin <$> pLitParts
pLitParts = do
  isEnd <- ('0' ==) <$> binDigit
  part <- replicateM 4 binDigit
  (part ++) <$> if isEnd then pure [] else pLitParts

hexToBin :: Char -> [Char]
hexToBin =
  reverse . take 4 . reverse . ("0000" ++) . ($ "")
    . showIntAtBase 2 intToDigit
    . fst
    . head
    . readHex
    . (: [])

{- Part one -}
solve1 (Packet v _ (Literal _)) = v
solve1 (Packet v _ (Operator ps)) = (v +) . sum . map solve1 $ ps

{- Part two -}
solve2 (Packet _ _ (Literal x)) = x
solve2 (Packet _ ty (Operator subpackets)) =
  let f = case ty of
        0 -> sum
        1 -> product
        2 -> minimum
        3 -> maximum
        5 -> (\[x, y] -> if x > y then 1 else 0)
        6 -> (\[x, y] -> if x < y then 1 else 0)
        7 -> (\[x, y] -> if x == y then 1 else 0)
   in f $ map solve2 subpackets

solutions =
  [ solve1
  , solve2
  ]

main :: IO ()
main = do
  idx <- read . head <$> getArgs
  print
    . (solutions !! (idx - 1))
    . fst
    . fromMaybe (error "Unrecognized string")
    . runParser pPacket
    . concatMap hexToBin
    =<< getContents
