module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , move
    , moveOne
    , changeDirection
    ) where

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show, Enum)

data Pos = Pos Integer Integer deriving (Eq, Show)

data Robot = Robot Bearing Pos deriving (Eq, Show)

bearing :: Robot -> Bearing
bearing (Robot b _) = b

coordinates :: Robot -> (Integer, Integer)
coordinates (Robot _ (Pos x y)) = (x, y)

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot direction (x, y) = Robot direction (Pos x y)

changeDirection :: Robot -> Char -> Robot
changeDirection (Robot dir pos) char
  | char == 'R' =
    case dir of
      West -> Robot North pos
      _    -> Robot (succ dir) pos
  | char == 'L' =
    case dir of
      North -> Robot West pos
      _     -> Robot (pred dir) pos
  | otherwise = Robot dir pos

moveOne :: Robot -> Char -> Robot
moveOne (Robot dir (Pos x y)) 'A' =
  case dir of
    North -> Robot dir (Pos x (y + 1))
    East  -> Robot dir (Pos (x + 1) y)
    South -> Robot dir (Pos x (y - 1))
    West  -> Robot dir (Pos (x - 1) y)
moveOne r _ = r

move :: Robot -> String -> Robot
move robot instructions = foldl reducer robot instructions
  where
    reducer :: Robot -> Char -> Robot
    reducer acc x = moveOne (changeDirection acc x) x
