module ProteinTranslation (proteins) where

import Control.Monad (join)
import Data.List.Split (chunksOf)
import qualified Data.Map as Map
import Data.Maybe (isJust)

data Codon
  = AUG
  | UUU
  | UUA
  | UUC
  | UUG
  | UCU
  | UCC
  | UCA
  | UCG
  | UAU
  | UAC
  | UGU
  | UGC
  | UGG
  | UAA
  | UAG
  | UGA
  deriving (Eq, Show, Ord)

data Protein
  = Methionine
  | Phenylalanine
  | Leucine
  | Serine
  | Tyrosine
  | Cysteine
  | Tryptophan
  deriving (Eq, Ord, Show)

hash :: Map.Map Codon (Maybe Protein)
hash =
  Map.fromList
    [ (AUG, Just Methionine),
      (UUU, Just Phenylalanine),
      (UUC, Just Phenylalanine),
      (UUA, Just Leucine),
      (UUG, Just Leucine),
      (UCU, Just Serine),
      (UCC, Just Serine),
      (UCA, Just Serine),
      (UCG, Just Serine),
      (UAU, Just Tyrosine),
      (UAC, Just Tyrosine),
      (UGU, Just Cysteine),
      (UGC, Just Cysteine),
      (UGG, Just Tryptophan),
      (UAA, Nothing),
      (UAG, Nothing),
      (UGA, Nothing)
    ]

stringToCodon :: String -> Codon
stringToCodon "AUG" = AUG
stringToCodon "UUU" = UUU
stringToCodon "UUC" = UUC
stringToCodon "UUA" = UUA
stringToCodon "UUG" = UUG
stringToCodon "UCU" = UCU
stringToCodon "UCC" = UCC
stringToCodon "UCA" = UCA
stringToCodon "UCG" = UCG
stringToCodon "UAU" = UAU
stringToCodon "UAC" = UAC
stringToCodon "UGU" = UGU
stringToCodon "UGC" = UGC
stringToCodon "UGG" = UGG
stringToCodon "UAA" = UAA
stringToCodon "UAG" = UAG
stringToCodon "UGA" = UGA
stringToCodon _ = error "Unknown Codon"

proteinToString :: Protein -> String
proteinToString = show

proteins :: String -> Maybe [String]
proteins input =
  (traverse . fmap $ proteinToString) . takeWhile isJust . map (\s -> join $ Map.lookup (stringToCodon s) hash) . chunksOf 3 $ input
