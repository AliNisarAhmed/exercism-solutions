module BST
  ( BST,
    bstLeft,
    bstRight,
    bstValue,
    empty,
    fromList,
    insert,
    singleton,
    toList,
  )
where

data BST a
  = Node a (BST a) (BST a)
  | Leaf
  deriving (Eq, Show)

bstLeft :: BST a -> Maybe (BST a)
bstLeft (Node _ l _) = Just l
bstLeft Leaf = Nothing

bstRight :: BST a -> Maybe (BST a)
bstRight (Node _ _ r) = Just r
bstRight Leaf = Nothing

bstValue :: BST a -> Maybe a
bstValue (Node v _ _) = Just v
bstValue Leaf = Nothing

empty :: BST a
empty = Leaf

fromList :: Ord a => [a] -> BST a
fromList = foldl (flip insert) empty

insert :: Ord a => a -> BST a -> BST a
insert x Leaf = singleton x
insert x (Node v l r)
  | x > v = Node v l (insert x r)
  | otherwise = Node v (insert x l) r

singleton :: a -> BST a
singleton x = Node x Leaf Leaf

toList :: BST a -> [a]
toList Leaf = []
toList (Node v l r) = toList l <> [v] <> toList r
