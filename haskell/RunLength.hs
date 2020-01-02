module RunLength (decode, encode) where

import           Data.Char (isDigit)
import           Data.List (group)

decode :: String -> String
decode "" = ""
decode text@(x: xs)
  | not $ isDigit x = [x] ++ decode xs
  | otherwise = (replicate (read digits) first) ++ decode rest
  where
    (digits, (first: rest)) = span isDigit text

encode :: String -> String
encode = concatMap replaceWithCount . group

replaceWithCount :: String -> String
replaceWithCount "" = ""
replaceWithCount str@(x:_)
  | count == 1 = [x]
  | otherwise = (show count) ++ [x]
  where
    count = length str
