module School (School, add, empty, grade, sorted) where

import Data.List (sort, sortOn)

type Grade = Int
type Student = String

newtype School = School [(Grade, [Student])] deriving (Eq, Show)

add :: Grade -> Student -> School -> School
add gg ss (School []) = School [(gg, [ss])]
add gradeNum student (School xs) = go (School []) xs
  where
    go (School acc) [] = School ( (gradeNum, [student]): acc )
    go (School acc) ( ((cg, cs):rem) ) =
      if cg == gradeNum
      then School ( (cg, cs ++ [student]):acc )
      else go (School ((cg, cs):acc)) rem

empty :: School
empty = School []

grade :: Grade -> School -> [Student]
grade _ (School []) = []
grade gradeNum (School (x:xs)) =
  if gradeNum == fst x
    then snd x
    else grade gradeNum (School xs)

sorted :: School -> [(Grade, [Student])]
sorted (School list) = (fmap . fmap) sort . sortOn fst $ list