module Queens (boardString, canAttack) where

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black = unlines $ fmap (printRow white black) [0..7]

printCol :: Maybe (Int, Int) -> Maybe (Int, Int) -> (Int, Int) -> String
printCol (Just (x, y)) _ (a, b)
  | x == a && y == b = "W"
printCol _ (Just (x, y)) (a, b)
  | x == a && y == b = "B"
printCol _ _ _ = "_"

printRow :: Maybe (Int, Int) -> Maybe (Int, Int) -> Int -> String
printRow w b rowNum = unwords $ fmap (\c -> printCol w b (rowNum, c)) [0 .. 7]

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack queenA queenB = 
  checkDiagonal queenA queenB || checkRowOrCol queenA queenB

checkDiagonal :: (Int, Int) -> (Int, Int) -> Bool
checkDiagonal (x1, y1) (x2, y2) = abs (x1 - x2) == abs (y1 - y2)

checkRowOrCol :: (Int, Int) -> (Int, Int) -> Bool 
checkRowOrCol (x1, y1) (x2, y2) = x1 == x2 || y1 == y2
