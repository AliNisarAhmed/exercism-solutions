module Clock (addDelta, fromHourMin, toString) where

newtype Hour = Hour Int deriving (Eq)

newtype Minute = Minute Int deriving (Eq)

hourToString :: Hour -> String
hourToString (Hour h) = replicate (2 - length s) '0' ++ s
  where
    s = show h

minuteToString :: Minute -> String
minuteToString (Minute m) = replicate (2 - length s) '0' ++ s
  where
    s = show m

-- ---------------

data Clock = Clock Hour Minute
  deriving (Eq)

fromHourMin :: Int -> Int -> Clock
fromHourMin hour minutes =
  Clock (Hour $ finalHour) (Minute $ mm)
  where
    hm = div minutes 60
    mm = mod minutes 60
    remHour = rem hour 24
    finalHour = mod (remHour + hm) 24

toString :: Clock -> String
toString (Clock hour minute) =
  hourToString hour ++ ":" ++ minuteToString minute

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour minute (Clock (Hour h) (Minute m)) =
  fromHourMin (h + hour) (minute + m)