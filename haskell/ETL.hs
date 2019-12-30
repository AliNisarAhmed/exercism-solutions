module ETL (transform) where

import qualified Data.Map as M
import Data.Map (Map)
import Data.Char (toLower)

-- This most likely is O(m n)
-- where m = size of map, n = size of string values

-- transform :: Map a String -> Map Char a
-- transform = M.fromList . concatMap func . M.toList
--   where
--     func (n, [x]) = [(toLower x, n)]
--     func (n, x:xs) = (toLower x, n) : func (n, xs)


-- foldrWithKey is O(n), insert is O(log n), union is O(n + m)
-- and for each entry in the map of type (k, v) where v is a string of length m
-- we perform insert
-- I estimate the run-time to be O(n log n)

transform :: Map a String -> Map Char a
transform = M.foldrWithKey' func M.empty
  where
		func k [x] acc = M.insert (toLower x) k acc
		func k (x:xs) acc = M.insert (toLower x) k acc `M.union` func k xs acc
