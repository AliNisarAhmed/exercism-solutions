module Luhn (isValid) where

import Data.Char (digitToInt, isDigit)
import Data.List (unfoldr)

isValid :: String -> Bool
isValid n
  | length sanitized <= 1 = False
  | otherwise = (== 0) . (`mod` 10) . sum . odds doubleMod9 . reverse $ sanitized
  where
    sanitized = digits . keepNums $ n

digits :: [Char] -> [Int]
digits = map digitToInt

keepNums :: [Char] -> [Char]
keepNums = filter isDigit

doubleMod9 :: Int -> Int
doubleMod9 n
  | d > 9 = d - 9
  | otherwise = d
  where
    d = n * 2

odds :: (a -> a) -> [a] -> [a]
odds f (x : y : xs) = [x, f y] ++ odds f xs
odds _ [x] = [x]
odds _ _ = []


-- other useful function that could simplify the above solution

-- Converts to a list of individual digits of a number but in reverse order
digits2 :: Int -> [Int]
digits2 = unfoldr f 
  where 
    f 0 = Nothing 
    f n = Just (mod n 10, div n 10)

doubleAtEvens :: [Int] -> [Int]
doubleAtEvens = zipWith (*) (cycle [1, 2])
