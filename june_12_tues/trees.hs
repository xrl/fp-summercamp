data Tree a = Empty
            | Node (Tree a) a (Tree a)
  deriving (Show)

treeSize :: Tree a -> Integer
treeSize Empty = 0
treeSize (Node treeL _ treeR) = 1 + (treeSize treeL) + (treeSize treeR)

{-
  [1,2] => Node (Node Empty 2 Empty) 1 (Empty)
-}
tree :: [a] -> Tree a
tree []      = Empty
tree (x:xs)  = addToTree x (tree xs)

addToTree :: a -> Tree a -> Tree a
addToTree x (Empty) = Node Empty x Empty
addToTree x (Node l y r)
  | treeSize l > treeSize r = Node l               y (addToTree x r)
  | otherwise               = Node (addToTree x l) y r

sampleTree =
  Node (Node Empty 1 Empty) 1 (Empty)
