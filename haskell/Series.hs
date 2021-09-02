module Series (slices) where

import Data.Char (digitToInt)
import Data.List (unfoldr)


digits :: String -> [Int]
digits = fmap digitToInt

slices :: Int -> String -> [[Int]]
slices 0 xs = [] : fmap (const []) xs
slices n xs = unfoldr step xs
  where 
  step c = 
    if length c < n 
      then Nothing 
      else Just (digits $ take n c, drop 1 c)
