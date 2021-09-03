module Minesweeper where

import Data.Char (intToDigit)
import Data.Foldable (toList)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

type Row = Int

type Col = Int


annotate :: [String] -> [String]
annotate board =
  toList $
    toList
      <$>
      -- Seq.mapWithIndex (\rowIndex rowItem -> Seq.mapWithIndex (step rowIndex) rowItem) seqs
      Seq.mapWithIndex (Seq.mapWithIndex . step) seqs
  where
    seqs = Seq.fromList (fmap Seq.fromList board)
    step :: Row -> Col -> Char -> Char
    step _ _ '*' = '*'
    step r c char =
      case countMines (getNeighbour seqs (r, c)) of
        0 -> char
        n -> intToDigit n

getNeighbourIndices :: (Row, Col) -> [(Row, Col)]
getNeighbourIndices (r, c) =
  [ (r -1, c -1),
    (r -1, c),
    (r -1, c + 1),
    (r, c -1),
    (r, c + 1),
    (r + 1, c -1),
    (r + 1, c),
    (r + 1, c + 1)
  ]

getNeighbour :: Seq (Seq Char) -> (Row, Col) -> [Maybe Char]
getNeighbour xs (r, c) =
  let ns = getNeighbourIndices (r, c)
   in do
        (r1, c1) <- ns
        pure $ do
          row <- Seq.lookup r1 xs
          Seq.lookup c1 row

countMines :: [Maybe Char] -> Int
countMines = foldr step 0
  where
    step (Just '*') acc = acc + 1
    step _ acc = acc
