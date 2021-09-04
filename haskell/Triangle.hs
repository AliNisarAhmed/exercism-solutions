module Triangle (rows) where

rows :: Int -> [[Integer]]
rows n = take n $ iterate nextRow [1]

nextRow :: [Integer] -> [Integer]
nextRow xs = 1 : zipWith (+) xs (tail xs) ++ [1]
