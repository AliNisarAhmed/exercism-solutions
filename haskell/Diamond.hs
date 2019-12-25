module Diamond (diamond) where

import           Data.Char (isAlpha, ord)

diamond :: Char -> Maybe [String]
diamond c
  | isAlpha c =
    Just $ map (padWithSpaces size . insertSpacesBetween) $ alphabetList c
  | otherwise = Nothing
  where
    size = width c

alphaNumber :: Char -> Int
alphaNumber c = ord c - 64

width :: Char -> Int
width c = alphaNumber c * 2 - 1

alphabetList :: Char -> [Char]
alphabetList c = xs ++ (tail . reverse) xs
  where
    xs = ['A' .. c]

getSpaces :: Int -> [Char]
getSpaces n = take n $ repeat ' '

numSpaces :: Char -> Int
numSpaces c = 2 * (alphaNumber c - 1) - 1

insertSpacesBetween :: Char -> String
insertSpacesBetween c
  | c == 'A' = "A"
  | otherwise = [c] ++ getSpaces (numSpaces c) ++ [c]

padWithSpaces :: Int -> String -> String
padWithSpaces n str
  | length str < n = padWithSpaces n $ " " ++ str ++ " "
  | otherwise = str
