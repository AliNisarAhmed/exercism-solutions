module Yacht where

import Data.List (sort)

data Category = Ones
              | Twos
              | Threes
              | Fours
              | Fives
              | Sixes
              | FullHouse
              | FourOfAKind
              | LittleStraight
              | BigStraight
              | Choice
              | Yacht

yacht :: Category -> [Int] -> Int
yacht Yacht list          = scoreYacht list
yacht Choice list         = sum list
yacht BigStraight list    = scoreBigStraight $ sort list
yacht LittleStraight list = scoreLittleSt8 $ sort list
yacht FullHouse list      = scoreFullHouse $ sort list
yacht FourOfAKind list    = scoreFourOfAKind $ sort list
yacht x list              = scoreSingles (categoryToInt x) list

scoreYacht :: [Int] -> Int
scoreYacht (a:b:c:d:e:_) =
  if (a == b && b == c && c == d && d == e)
    then 50
    else 0

scoreBigStraight :: [Int] -> Int
scoreBigStraight (2:3:4:5:6:_) = 30
scoreBigStraight _ = 0

scoreLittleSt8 :: [Int] -> Int
scoreLittleSt8 (1:2:3:4:5:_) = 30
scoreLittleSt8 _ = 0

scoreFourOfAKind :: [Int] -> Int
scoreFourOfAKind (a:b:c:d:e:_) =
  if (a == b && b == c && c == d) || (b == c && c == d && d == c)
    then 4 * b
    else 0

scoreFullHouse :: [Int] -> Int
scoreFullHouse (a:b:c:d:e:_) =
  if (a == b && b == c && d == e && c /= d) ||
    (a == b && c == d && d == e && b /= c)
    then a + b + c + d + e
    else 0

categoryToInt :: Category -> Int
categoryToInt Ones = 1
categoryToInt Twos = 2
categoryToInt Threes = 3
categoryToInt Fours = 4
categoryToInt Fives = 5
categoryToInt Sixes = 6
categoryToInt _ = 0

scoreSingles :: Int -> [Int] -> Int
scoreSingles n list = sum $ filter (== n) list
