module ResistorColors
  ( Color(..)
  , Resistor(..)
  , label
  , ohms
  )
where

data Color =
    Black
  | Brown
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Violet
  | Grey
  | White
  deriving (Show, Enum, Bounded)

newtype Resistor = Resistor { bands :: (Color, Color, Color) }
  deriving Show

getColorValue :: Color -> Int
getColorValue Black  = 0
getColorValue Brown  = 1
getColorValue Red    = 2
getColorValue Orange = 3
getColorValue Yellow = 4
getColorValue Green  = 5
getColorValue Blue   = 6
getColorValue Violet = 7
getColorValue Grey   = 8
getColorValue White  = 9

label :: Resistor -> String
label resistor = show value ++ ohmLabel
 where
  ohmValue          = ohms resistor
  (value, ohmLabel) = getValueAndLabel ohmValue

ohms :: Resistor -> Int
ohms (Resistor (color1, color2, color3)) =
  getBandValue color1 color2 * (numberOfZeroes color3)

getBandValue :: Color -> Color -> Int
getBandValue c1 c2 = v1 * 10 + v2
 where
  v1 = getColorValue c1
  v2 = getColorValue c2

numberOfZeroes :: Color -> Int
numberOfZeroes c = 10 ^ getColorValue c

getValueAndLabel :: Int -> (Int, String)
getValueAndLabel x
  | x >= thousand && x < million = (x `div` thousand, " kiloohms")
  | x >= million && x < billion  = (x `div` million, " megaohms")
  | x > billion                  = (x `div` billion, " gigaohms")
  | otherwise                    = (x, " ohms")
 where
  thousand = 1000
  million  = 1000000
  billion  = 1000000000
