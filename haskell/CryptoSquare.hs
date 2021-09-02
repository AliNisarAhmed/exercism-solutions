module CryptoSquare (encode) where

import Data.Char (isAlphaNum, toLower)
import Data.List (transpose, unfoldr)
import Data.Bifunctor (first)

encode :: String -> String
encode xs = unwords $ transpose $ chunks c sanitized
  where
    sanitized = convertToLower . keepAlphaNum $ xs
    (c, _) = findFactors (length sanitized)

keepAlphaNum :: String -> String
keepAlphaNum = filter isAlphaNum

convertToLower :: String -> String
convertToLower = fmap toLower

findFactors :: Int -> (Int, Int)
findFactors n = go 1 1
  where
    go j k
      | j * k >= n = (j, k)
      | j > k = go j (k + 1)
      | otherwise = go (j + 1) k

chunks :: Int -> [Char] -> [[Char]]
chunks n = takeWhile (not . null) . unfoldr (Just . first (padRight n) . splitAt n)

padRight :: Int -> String -> String
padRight n s
  | len == 0 = s
  | len >= n = s
  | otherwise = take n (s ++ repeat ' ')
  where 
    len = length s

