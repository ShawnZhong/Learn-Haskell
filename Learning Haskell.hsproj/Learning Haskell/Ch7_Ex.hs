module Ch7_Ex where
  
import Ch7_2
import ShapeGraphics
  
-- Exercise 1
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x


safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (x:xs)    = Just xs



-- Exercise 2
myLength :: [a] -> Int
myLength list = myLength' (safeTail list) where
  myLength' :: Maybe [a] -> Int
  myLength' Nothing = 0
  myLength' (Just x) = 1 + myLength x
  



-- Exercise 3
deleteSorted :: Ord a => a -> [a] ->  [a]
deleteSorted v [] = []
deleteSorted v (x:xs)
  | x == v = xs
  | otherwise = x : deleteSorted v xs


-- Exercise 4
deleteSmallestTree :: Ord a => BinaryTree a -> BinaryTree a
deleteSmallestTree (Node v Leaf Leaf) = Leaf
deleteSmallestTree (Node v Leaf r)    = r
deleteSmallestTree (Node v l r)       = Node v (deleteSmallestTree l) r





-- Exercise 5
treeToList :: BinaryTree a -> [a]
treeToList Leaf         = []
treeToList (Node v l r) = treeToList l ++ [v] ++ treeToList r




-- Exercise 6
findSmallestTree :: Ord a => BinaryTree a -> a
findSmallestTree (Node v Leaf r) = v
findSmallestTree (Node v l r)    = findSmallestTree l

deleteTree :: Ord a => a -> BinaryTree a -> BinaryTree a
deleteTree x Leaf = Leaf
deleteTree x n@(Node v Leaf Leaf)
  | x == v    = Leaf
deleteTree x n@(Node v Leaf r)
  | x == v    = r
  | x > v     = Node v Leaf (deleteTree x r)
deleteTree x n@(Node v l Leaf)
  | x == v    = l
  | x < v     = Node v (deleteTree x l) Leaf
deleteTree x n@(Node v l r)
  | x == v    = Node (findSmallestTree r) l (deleteSmallestTree r)
  | x < v     = Node v (deleteTree x l) r
  | x > v     = Node v l (deleteTree x r)








