module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

listOfFactors :: Int -> [Int]
listOfFactors x = [ y | y <- [1..div x 2], mod x y == 0 ]

classify :: Int -> Maybe Classification
classify x
  | x <= 0 = Nothing
  | sumOfFactors < x = Just Deficient
  | sumOfFactors == x = Just Perfect
  | otherwise = Just Abundant
    where
      sumOfFactors = sum . listOfFactors $ x
