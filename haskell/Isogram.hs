module Isogram (isIsogram) where

import qualified Data.Map.Strict as M
import Data.Char (toLower)

empty :: M.Map Char Int
empty = M.fromList []

filterOutSpacesAndDashes :: String -> String
filterOutSpacesAndDashes = map toLower . filter (\x -> x /= ' ' && x /= '-')

countLetters :: String -> M.Map Char Int
countLetters = foldr reducer empty
  where
    reducer :: Char -> M.Map Char Int -> M.Map Char Int
    reducer x m = M.insertWith (+) x 1 m

isIsogram :: String -> Bool
isIsogram str = M.foldr (\x acc -> if x < 2 then acc else False) True builtMap
    where
      builtMap = countLetters . filterOutSpacesAndDashes $ str