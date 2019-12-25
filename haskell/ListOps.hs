module ListOps
  ( length
  , reverse
  , map
  , filter
  , foldr
  , foldl'
  , (++)
  , concat
  ) where

import Prelude hiding
  ( length, reverse, map, filter, foldr, (++), concat )

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ z [] = z
foldl' f z (x:xs) = -- seq (f z x) $ foldl' f (f z x) xs
  seq z' $ foldl' f z' xs
  where
    z' = f z x
-- seq :: x -> y -> y  evaluates x first, then returns y
-- The idea is that y references x so that
--   when y is reduced x will not be a big unreduced chain anymore.

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ z [] = z
foldr f z (x:xs) = f x (foldr f z xs)

length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + length xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter f (x:xs)
  | f x       = x : filter f xs
  | otherwise = filter f xs

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
xs ++ [] = xs
(x:xs) ++ ys = x: xs ++ ys

concat :: [[a]] -> [a]
concat [] = []
concat (x:xs) = x ++ concat xs
