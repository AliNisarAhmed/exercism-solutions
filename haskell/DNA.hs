module DNA where

import Data.Map (Map, fromList, insertWith)
import Text.Read (readEither)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show, Read)

empty :: Map Nucleotide Int
empty = fromList [(A, 0), (C, 0), (G, 0), (T, 0)]

charToNucleotide :: Char -> Either String Nucleotide
charToNucleotide 'A' = Right A
charToNucleotide 'C' = Right C
charToNucleotide 'G' = Right G
charToNucleotide 'T' = Right T
charToNucleotide  x  = Left $ "Error: " ++ "cannot read " ++ [x]

countOne :: Char -> Map Nucleotide Int -> Either String (Map Nucleotide Int)
countOne c nMap = do
  nuc <- charToNucleotide c
  return $ insertWith (+) nuc 1 nMap

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts = foldr reducer (Right empty)
  where
    reducer :: Char -> Either String (Map Nucleotide Int) -> Either String (Map Nucleotide Int)
    reducer c eitherMap = do
      m <- eitherMap
      countOne c m