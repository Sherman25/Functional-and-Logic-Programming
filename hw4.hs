-- /*****************
-- * Haskell Homework 4
-- *****************/

-- Question 1
-- a) Define naturals - an infinite sequence of the natural numbers
naturals = buildList 1 where buildList n = n : buildList (n+1)

-- b) Define squares - an infinite sequence of the natural numbers squared
squares = map (^2) naturals

-- c) Define threes - an infinite sequence of mulriplications of 3
threes = map (*3) naturals

-- d) Define res 
res = buildRes 0 where buildRes acc = naturals !! acc : squares !! acc : threes !! acc : buildRes (acc + 1)

-- e) implementation of switch function
switch :: [a] -> [a]
switch xs = let n = (reverse (take 2 xs)) 
			in n ++ switch (drop 2 xs) 
			
-- Question 2 - binary Tree
data BinaryTree a = Nil | BNode a (BinaryTree a) (BinaryTree a) deriving Show

-- a)
infTree :: a -> BinaryTree a
infTree n = BNode n (infTree n) (infTree n)

-- b)
treeMap :: (a -> b) -> BinaryTree a -> BinaryTree b
treeMap f (Nil) = Nil
treeMap f (BNode x l r) = BNode (f x) (treeMap f l) (treeMap f r)

-- c)
type Depth = Int  

treeTake :: Depth -> BinaryTree a -> BinaryTree a
treeTake 0 _ = Nil
treeTake _ Nil = Nil
treeTake 1 (BNode x l r) = (BNode x Nil Nil)
treeTake n (BNode x l r) = BNode x (treeTake (n-1) l) (treeTake (n-1) r)

-- d)
treeSort :: BinaryTree t -> [t]
treeSort Nil = []
treeSort (BNode n Nil Nil) = [n]
treeSort (BNode n l r) = treeSort l ++ [n] ++ treeSort r