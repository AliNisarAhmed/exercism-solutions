{-# LANGUAGE OverloadedStrings #-}

module WordCount (wordCount) where

import Data.Char (isAlphaNum, toLower)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T

wordCount :: Num a => Text -> Map Text a
wordCount xs = foldr step Map.empty sanitized
  where
    sanitized = filter (not . T.null) $ sanitize $ splitIntoWords xs
    step x acc = Map.insertWith (+) x 1 acc

splitIntoWords :: Text -> [Text]
splitIntoWords = T.split (`elem` delimiters)
  where
    delimiters = [' ', '\n', '\r', '\t', ',']

sanitize :: [Text] -> [Text]
sanitize = fmap (T.pack . map toLower . removeApost . filter validChars . T.unpack)
  where
    validChars c = c == '\'' || isAlphaNum c

removeApost :: [Char] -> [Char]
removeApost [] = []
removeApost str@(firstChar : s)
  | firstChar == '\'' && lastChar == '\'' = restOfString
  | otherwise = str
  where
    lastChar = last s
    restOfString = init s
