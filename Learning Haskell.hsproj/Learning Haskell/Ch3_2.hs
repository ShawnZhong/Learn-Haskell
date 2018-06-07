module Ch3_2 where

import Prelude hiding (reverse, product, concat)

-- Reductions: combining the elements of a list
-- Calculate the product of a list
product :: Num a => [a] -> a
product []     = 1


-- Find the minimum element in a list
minList :: [Int] -> Int
minList []     = maxBound
minList (x:xs) = min x (minList xs)



-- Concate two Strings
concat :: [a] -> [a] -> [a]
concat [] ys = ys
concat (x:xs) ys = x : (xs ++ ys)



-- Flatten a nested list
flatten :: [[a]] -> [a]
flatten [] = []
flatten (xs:xss) = xs ++ (flatten xss)



-- Put a element to the end of list
snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]



-- Reverse a list
reverse :: [a] -> [a]
reverse []     = []
reverse (x:xs) = reverse xs ++ [x]