module Tree
where

data Tree a = Node (Tree a) a (Tree a)
            | Empty
  deriving (Show)

size :: Tree a -> Integer
size Empty                = 0
size (Node treeL _ treeR) = 1 + (size treeL) + (size treeR)

tree :: (Ord a) => [a] -> Tree a
tree []      = Empty
tree (x:xs)  = add (tree xs) x

add :: (Ord a) => Tree a -> a -> Tree a
add (Empty) x      = Node Empty x Empty
add (Node l y r) x =
  case compare x y of
  LT -> Node (add l x) y r
  EQ -> undefined
  GT -> Node l y (add r x)

member :: (Eq a) => Tree a -> a -> Bool
member Empty x        = False
member (Node l y r) x =
  case x == y of
  True  -> True
  False -> member l x || member r x

memberS :: (Ord a, Eq a) => Tree a -> a -> Bool
memberS Empty x        = False
memberS (Node l y r) x =
  case compare x y of
  LT -> memberS l x
  EQ -> True
  GT -> memberS r x

inOrder :: Tree a -> [a]
inOrder Empty        = []
inOrder (Node l y r) = inOrder l ++ y : inOrder r

sample =
  Node (Node Empty 1 Empty) 1 (Empty)
