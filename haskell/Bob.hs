{-# LANGUAGE OverloadedStrings #-}

module Bob (responseFor) where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Char

isQuestion :: Text -> Bool
isQuestion text =
  case T.unsnoc text of
    Nothing -> False
    Just (_, c) -> c == '?'

isAllUpper :: Text -> Bool
isAllUpper text
  | T.null filtered = False
  | otherwise = T.all isUpper filtered
    where
      filtered = T.filter isAlpha text

responseFor :: Text -> Text
responseFor text
    | T.null stripped = "Fine. Be that way!"
    | isAQuestion && isAllUpper stripped
      = "Calm down, I know what I'm doing!"
    | isAQuestion = "Sure."
    | isAllUpper text = "Whoa, chill out!"
    | otherwise = "Whatever."
      where
        stripped = T.strip text
        isAQuestion = isQuestion stripped