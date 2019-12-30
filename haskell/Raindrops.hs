module Raindrops (convert) where

convert :: Int -> String
convert n
  | result == [] = show n
  | otherwise = result
  where
    result = checkFactors n

checkFactors :: Int -> String
checkFactors n = concatMap ($ n) $ [hasFactor 3, hasFactor 5, hasFactor 7]

hasFactor :: Int -> Int -> String
hasFactor 3 n | mod n 3 == 0 = "Pling"
hasFactor 5 n | mod n 5 == 0 = "Plang"
hasFactor 7 n | mod n 7 == 0 = "Plong"
hasFactor _ _ = ""
