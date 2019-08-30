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
  | filtered == "" = False
  | otherwise = all isUpper filtered
    where
      filtered = filter isAlpha (T.unpack text)

responseFor :: Text -> Text
responseFor text =
  if stripped == T.empty
  then T.pack "Fine. Be that way!"
  else
    if isQuestion stripped && isAllUpper stripped
    then T.pack "Calm down, I know what I'm doing!"
    else
      if isQuestion stripped
      then T.pack "Sure."
      else
        if isAllUpper text
        then T.pack "Whoa, chill out!"
        else T.pack "Whatever."
  where
    stripped = T.strip text