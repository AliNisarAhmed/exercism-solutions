module IsbnVerifier (isbn) where

import Data.Char (isAlphaNum, isDigit, digitToInt)

isbn :: String -> Bool
isbn str =
  if checkValidity r
    then checkIsbnScore r
    else False
  where
    r = reverse $ filter isAlphaNum str

checkValidity :: String -> Bool
checkValidity []           = False
checkValidity str@(h:rest) =
  length str == 10 && (h == 'X' || isDigit h) && all isDigit rest

checkIsbnScore :: String -> Bool
checkIsbnScore str =
  mod (sum $ zipWith (*) [1..10] list) 11 == 0
    where
      list = map convertToNumber str

convertToNumber :: Char -> Int
convertToNumber 'X' = 10
convertToNumber c   = digitToInt c