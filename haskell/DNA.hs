module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA xs = traverse convertToRNA xs

convertToRNA :: Char -> Either Char Char
convertToRNA c =
  case c of
    'G' -> Right 'C'
    'C' -> Right 'G'
    'T' -> Right 'A'
    'A' -> Right 'U'
    x   -> Left x