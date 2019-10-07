module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n
  | n <= 0 = Nothing
  | otherwise = Just $ fromIntegral . length . takeWhile (/= 1) $ collatzSteps n

collatzSteps :: Integer -> [Integer]
collatzSteps = iterate nextCollatzStep

nextCollatzStep :: Integer -> Integer
nextCollatzStep n
    | even n = div n 2
    | otherwise = 3 * n + 1