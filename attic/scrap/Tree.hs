module BST
( empty
, singleton
, fromList
, fromListBy
, insert
, insertBy
, member
, memberBy
, height
, size
, item
, left
, right
) where

data Tree a = Empty
            | Node a (Tree a) (Tree a)
            deriving (Read, Show, Eq, Ord)

instance BinTree BST where
  item Empty                = Nothing
  item (Node x left right)  = Just x

  left Empty                = Nothing
  left (Node x left right)  = Just left

  right Empty               = Nothing
  right (Node x left right) = Just right

instance NTree BST where
  item Empty                = Nothing
  item (Node x left right)  = Just x

  children Empty               = []
  children (Node x left right) = [left, right]

-- Construct an empty tree
empty :: Tree a
empty = Empty

-- Constructe a one-element tree
singleton :: a -> Tree a
singleton x = Node x empty empty

-- Construct a tree from a list of items
fromListBy :: (a -> a -> Ordering) -> [a] -> Tree a
fromListBy rel xs = foldr (insertBy rel) empty xs

-- Construct a tree from a list of items
fromList :: (Ord a) => [a] -> Tree a
fromList = fromListBy compare

insertBy :: (a -> a -> Ordering) -> a -> Tree a -> Tree a
insertBy rel x Empty = singleton x
insertBy rel x (Node y left right)
  | x `rel` y == LT = Node y (insertBy rel x left) right
  | x `rel` y == GT = Node y left (insertBy rel x right)
  | x `rel` y == EQ = Node y left right

insert :: (Ord a) => a -> Tree a -> Tree a
insert = insertBy compare

memberBy :: (a -> a -> Ordering) -> a -> Tree a -> Bool
memberBy rel x Empty = False
memberBy rel x (Node y left right)
  | x `rel` y == EQ = True
  | x `rel` y == LT = memberBy rel x left
  | x `rel` y == GT = memberBy rel x right

member :: (Ord a) => a -> Tree a -> Bool
member = memberBy compare

height :: Tree a -> Int
height Empty               = 0
height (Node _ left right) = 1 + max (height left) (height right)

size :: Tree a -> Int
size Empty               = 0
size (Node _ left right) = 1 + (size left) + (size right)
