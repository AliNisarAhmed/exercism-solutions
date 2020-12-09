module Matrix where

import Data.Array (Array)
import qualified Data.Array as A
import Data.Foldable (Foldable (foldl'))

saddlePoints :: Array (Int, Int) Int -> [(Int, Int)]
saddlePoints matrix = reverse $ foldl' step [] assocs
  where
    assocs = A.assocs matrix
    step acc ((r, c), n) =
      if isSaddlePoint n (filterSameRow r assocs) (filterSameColumn c assocs)
        then (r, c) : acc
        else acc

filterSameRow :: Int -> [((Int, Int), Int)] -> [Int]
filterSameRow row = map snd . filter ((== row) . fst . fst)

filterSameColumn :: Int -> [((Int, Int), Int)] -> [Int]
filterSameColumn col = map snd . filter ((== col) . snd . fst)

isSaddlePoint :: Int -> [Int] -> [Int] -> Bool
isSaddlePoint _ [] _ = True
isSaddlePoint _ _ [] = True
isSaddlePoint n rowElems colElems = maximum rowElems == n && minimum colElems == n

--- better solution

-- saddlePoints :: Ord e => Array (Int, Int) e -> [(Int, Int)]
-- saddlePoints matrix = map fst $ filter isSaddlePoint $ assocs matrix
--  where
--   --  isSaddlePoint works in constant time
--   isSaddlePoint ((x, y), value) = value == rowMaxima ! x && value == colMinima ! y
--   ((x1, y1), (x2, y2))          = bounds matrix
--   row i                         = map (\y -> matrix ! (i, y)) [y1 .. y2]
--   col j                         = map (\x -> matrix ! (x, j)) [x1 .. x2]
--   -- each maximum and minimum is calculated only once
--   rowMaxima                     = listArray (x1, x2) $ map (maximum . row) [x1 .. x2]
--   colMinima                     = listArray (y1, y2) $ map (minimum . col) [y1 .. y2]