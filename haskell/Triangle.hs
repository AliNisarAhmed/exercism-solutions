module Triangle (TriangleType(..), triangleType) where

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

triangleType :: (Num a, Ord a) => a -> a -> a -> TriangleType
triangleType a b c
  | isInvalidTriangle a b c    = Illegal
  | a == b && b == c && c == a = Equilateral
  | a == b || b == c || c == a = Isosceles
  | otherwise                  = Scalene

isInvalidTriangle :: (Num a, Ord a) => a -> a -> a -> Bool
isInvalidTriangle a b c =
  a <= 0 || b <= 0 || c <= 0 || (a + b <= c) || (b + c <= a) || (a + c <= b)