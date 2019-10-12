module Phone (number) where

import Data.Char (isDigit)


number :: String -> Maybe String
number xs
  | len == 11 = checkHeadIsOne digitsOnly
  | len == 10 = checkValid digitsOnly
  | otherwise = Nothing
  where
    digitsOnly                 = filter isDigit xs
    len                        = length digitsOnly

validNumbers :: String
validNumbers = ['2'..'9']

checkHeadIsOne :: String -> Maybe String
checkHeadIsOne ('1':ys) = checkValid ys
checkHeadIsOne _        = Nothing

checkValid :: String -> Maybe String
checkValid num@(a:_:_:b:_) =
  if elem a validNumbers && elem b validNumbers
    then Just num
    else Nothing
