module Ch7_2 where
  

import Codec.Picture
import ShapeGraphics

-- List
data List a = Cons a (List a) | Nil

isElement :: Eq a => a -> [a] -> Bool
isElement _ []     = False
isElement a (x:xs)
  | a == x         = True
  | otherwise      = isElement a xs


-- precondition: list is sorted in increasing order
isElementSorted :: Ord a  => a -> [a] -> Bool
isElementSorted _ []     = False
isElementSorted a (x:xs)
  | a == x               = True
  | a < x                = False
  | otherwise            = isElementSorted a xs


-- precondition: list is sorted in increasing order
-- postcondition: return list is sorted in increasing order
insertSorted :: (Eq a, Ord a)  => a -> [a] -> [a]
insertSorted x []     = [x]
insertSorted x (y:ys) 
  | x <= y    = x : y : ys
  | otherwise = y : insertSorted x ys






--  Binary Trees

data BinaryTree a = Node a (BinaryTree a) (BinaryTree a) | Leaf deriving (Show)


-- precondition: tree is sorted in increasing order
-- postcondition: return tree is sorted in increasing order
insertTree :: Ord a => a -> BinaryTree a -> BinaryTree a
insertTree x Leaf = Node x Leaf Leaf
insertTree x (Node v l r)
  | x < v     = Node v (insertTree x l) r
  | otherwise = Node v l (insertTree x r)



-- precondition: tree is sorted in increasing order
isElementTree :: Ord a => a -> BinaryTree a -> Bool
isElementTree x Leaf = False
isElementTree x (Node v l r)
  | x == v    = True     
  | x < v     = isElementTree x l 
  | otherwise = isElementTree x r




listToTree :: Ord a => [a] -> BinaryTree a
listToTree [] = Leaf
listToTree (x:xs) = insertTree x (listToTree xs)







-- New Types

newtype Celsius    = Celsius    Float
newtype Fahrenheit = Fahrenheit Float
-- Better performance compared to
-- data Celsius    = Celsius    Float
-- data Fahrenheit = Fahrenheit Float














-- Render a binary tree as a 'ShapeGraphics' picture.
--
renderTree :: Show a => BinaryTree a -> Picture
renderTree tree = movePicture (Vector ((1000 - w)/2) ((1000 - h)/2)) p1
  where 
    dist     = 40
    nodeSize = 50
    height   = 100
    (p1, x, h, w) = renderTree' tree
    renderTree' Leaf 
      = (leafPoly, nodeSize/2, nodeSize, nodeSize)         
    renderTree' (Node x t1 t2) 
      = ([Path [Point (width1   -5) 50, Point t1x 100]  black Solid] ++ 
         [Path [Point (width1 + 45) 50, Point (width1 + t2x + 40) 100] black Solid] ++ 
          movePicture (Vector (width1 + dist/2) 0) (nodePic x) ++
          movePicture (Vector 0                 height) t1Pic  ++
          movePicture (Vector (width1 + dist)   height) t2Pic,
         width1 + dist/2, 
         height + max height1 height2, 
         width1 + width2 + dist) 
      where
        (t1Pic, t1x, height1, width1) = renderTree' t1
        (t2Pic, t2x, height2, width2) = renderTree' t2
    nodePic x  
      = let
          size = fromIntegral $ length $ show x
        in [TextBox (Point (- size * 10) 40) black (show x) (nodeSize/2), 
            Polygon [Point (- nodeSize/2) 0, Point (-nodeSize/2) nodeSize, 
                     Point (nodeSize/2) nodeSize, Point (nodeSize/2) 0, 
                     Point 0 0] 
                    black Solid NoFill]
    leafPoly :: Picture
    leafPoly = [Polygon [Point 0 0, Point 0 nodeSize, Point nodeSize nodeSize, 
                         Point nodeSize 0, Point 0 0, Point nodeSize nodeSize, 
                         Point 0 nodeSize, Point nodeSize 0] black Solid NoFill]