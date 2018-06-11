module Ch5_1 where

import Data.Char
import LineGraphics
import Prelude hiding (map, zipWith, filter, foldr, sum, concat, reverse, foldl, ($), (.))


-- map
map :: (a -> b) -> [a] -> [b]
map f []       = []
map f (x : xs) = f x : map f xs


allSquares :: Num a => [a] -> [a]
allSquares xs = map square xs where square x = x * x


allToUpper :: String -> String
allToUpper string = map toUpper string



distance :: Point -> Point -> Float
distance (x1, y1) (x2, y2) = sqrt(dx * dx + dy * dy) where dx = x1 - x2; dy = y1 - y2
distancesFromPoint :: Point -> [Point] -> [Float]
distancesFromPoint point points = map (distance point) points
-- Note that `distance point` is a function that takes another point to calculate the distanc between `point` and the other point






-- zipWith
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith op []       _        = []
zipWith op _        []       = []
zipWith op (x : xs) (y : ys) = (x `op` y) : zipWith op xs ys

average :: Fractional a => a -> a -> a
average x y = (x + y) / 2




-- filter
filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x : xs) | p x       = x : filter p xs
                  | otherwise = filter p xs
                  
extractDigits :: String -> String
extractDigits strings = filter isDigit strings

inRadius :: Point -> Float -> [Point] -> [Point]
inRadius point radius points = filter inRadiusP points
  where
    inRadiusP :: Point -> Bool
    inRadiusP p = distance point p <= radius




-- Anonymous functions
inRadius' :: Point -> Float -> [Point] -> [Point]
inRadius' point radius points = filter (\p -> distance point p <= radius) points

allSquares' :: Num a => [a] -> [a]
allSquares' xs  = map (\x -> x * x) xs


-- Point-free notation
inRadius'' :: Point -> Float -> [Point] -> [Point]
inRadius'' point radius  = filter (\p -> distance point p <= radius)

allSquares'' :: Num a => [a] -> [a]
allSquares'' = map (\x -> x * x)





-- foldr
-- Note: The return type of op shoule be the same as the type of second argument
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr op n [] = n
foldr op n (x:xs) = x `op` foldr op n xs


-- Note: We are using point-free notation
minList :: [Int] -> Int
minList = foldr min maxBound 

sum :: Num a => [a] -> a
sum = foldr (+) 0 

-- Note: Here op :: Int -> Bool -> Bool
allEven :: [Int] -> Bool
allEven = foldr (\x b -> even x && b) True

-- Example of foldr having non-scalar returning value
concat :: [[a]] -> [a]
concat = foldr (++) []

reverse :: [a] -> [a]
reverse = foldr (\x xs -> xs ++ [x]) []





-- foldl
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl op acc []     = acc
foldl op acc (x:xs) = foldl op (acc `op` x) xs

deductFromAccount :: Int -> [Int] -> Int
deductFromAccount initialBalance = foldl (-) initialBalance 

stringToInt :: String -> Int
stringToInt = foldl (\acc chr -> 10 * acc + digitToInt chr) 0

fastReverse :: [a] -> [a]
fastReverse = foldl (\accList x -> x : accList) []





-- Combining Multiple Recursive Patterns
-- Note: > 0 is the same as the lambda expression (\x -> x > 0)
sumOfSquareRoots :: (Ord a, Floating a) => [a] -> a
sumOfSquareRoots xs = sum (map sqrt (filter (> 0) xs))




-- Function application with lower precedence
infixr 0 $                  -- ($) is right associative and has precedence level 0
($) :: (a -> b) -> a -> b
f $ x = f x

sumOfSquareRoots' xs = sum $ map sqrt $ filter (> 0) xs





-- Function composition
--sumOfSquareRoots = sum $ map sqrt $ filter (> 0)  -- Error, since $ requires the last argument

-- Note: ($) is the abstraction of function application, while (.) is the abstraction of function composition
(.) :: (b -> c) -> (a -> b) -> (a -> c) 
(f . g) x = f (g x)

sumOfSquareRoots''  = sum . map sqrt . filter (> 0) 