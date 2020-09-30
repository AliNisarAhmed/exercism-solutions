module Base (Error (..), rebase) where

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
  deriving (Show, Eq)

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits
  | inputBase <= 1 = Left InvalidInputBase
  | outputBase <= 1 = Left InvalidOutputBase
  | all (== 0) inputDigits = Right []
  | otherwise = base10ToOther outputBase (convertToBase10 inputBase inputDigits) []
  where
    base10ToOther :: Integral a => a -> Either (Error a) a -> [a] -> Either (Error a) [a]
    base10ToOther _ (Left e) _ = Left e
    base10ToOther o (Right number) acc
      | number < o = return $ number : acc
      | otherwise = base10ToOther o (return $ div number o) (mod number o : acc)

convertToBase10 :: Integral a => a -> [a] -> Either (Error a) a
convertToBase10 base inputNumbers =
  fmap sum . traverse id $ zipWith zipper inputNumbers powers
  where
    len = length inputNumbers
    powers = [len - 1, len - 2 .. 0]
    zipper n p
      | n < 0 || n >= base = Left (InvalidDigit n)
      | otherwise = Right $ n * base ^ p