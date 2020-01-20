module ArmstrongNumbers (armstrong) where

import Data.Char (digitToInt)

armstrong :: Integral a => a -> Bool
armstrong x = (==) x (sum $ map (^ len) digitList)
	where
		len = length digitList
		digitList = digits x

digits :: Integral a => a -> [a]
digits 0 = []
digits x = (mod x 10) : digits (div x 10)
