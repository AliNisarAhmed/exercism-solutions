module PrimeFactors (primeFactors) where

import Data.List

primes :: [Integer]
primes = sieve [2 ..]

sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (p : xs) = p : sieve [x | x <- xs, x `rem` p /= 0]

primeFactors :: Integer -> [Integer]
primeFactors n = primeFactors' n primes

primeFactors' :: Integer -> [Integer] -> [Integer]
primeFactors' 1 _ = []
primeFactors' n primeList@(p : ps) =
  if n `rem` p == 0
    then p : primeFactors' (n `div` p) primeList
    else primeFactors' n ps
