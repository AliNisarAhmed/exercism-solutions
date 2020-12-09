module Anagram (anagramsFor) where

import Data.Char (toLower)

anagramsFor :: String -> [String] -> [String]
anagramsFor xs = filter filterFunc
  where
    wl = length xs
    lowered = map toLower xs
    filterFunc :: String -> Bool
    filterFunc word =
      length word == wl
        && lowerWord /= lowered
        && all (\v -> countChar v lowered == countChar v lowerWord) lowerWord
      where
        lowerWord = map toLower word

countChar :: Char -> String -> Int
countChar x = length . filter (== x)