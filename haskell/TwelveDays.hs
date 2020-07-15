module TwelveDays
  ( recite
  )
where

recite :: Int -> Int -> [String]
recite start stop = foldr step [] [start .. stop]
  where step v acc = getFullClause v : acc

-- a function of start
getFirstClause :: Int -> String
getFirstClause n =
  "On the " ++ getDay n ++ " day of Christmas my true love gave to me: "


getSecondClause :: Int -> String
getSecondClause n = foldr step "" [1 .. n]
 where
  step 1 acc | n > 1     = acc ++ "and " ++ getSubClause 1
             | otherwise = acc ++ getSubClause 1
  step m acc = acc ++ getSubClause m ++ ", "



getFullClause :: Int -> String
getFullClause n = getFirstClause n ++ getSecondClause n ++ "."

-- a function of start
getSubClause :: Int -> String
getSubClause 1  = "a Partridge in a Pear Tree"
getSubClause 2  = "two Turtle Doves"
getSubClause 3  = "three French Hens"
getSubClause 4  = "four Calling Birds"
getSubClause 5  = "five Gold Rings"
getSubClause 6  = "six Geese-a-Laying"
getSubClause 7  = "seven Swans-a-Swimming"
getSubClause 8  = "eight Maids-a-Milking"
getSubClause 9  = "nine Ladies Dancing"
getSubClause 10 = "ten Lords-a-Leaping"
getSubClause 11 = "eleven Pipers Piping"
getSubClause 12 = "twelve Drummers Drumming"


getDay :: Int -> String
getDay 1  = "first"
getDay 2  = "second"
getDay 3  = "third"
getDay 4  = "fourth"
getDay 5  = "fifth"
getDay 6  = "sixth"
getDay 7  = "seventh"
getDay 8  = "eighth"
getDay 9  = "ninth"
getDay 10 = "tenth"
getDay 11 = "eleventh"
getDay 12 = "twelfth"
