module HS5_Higher_order_Functions where

import Data.Char
import Prelude hiding (map, zipWith, filter)

-- map
map :: (a -> b) -> [a] -> [b]
map f []       = []
map f (x : xs) = f x : map f xs


allSquares :: Num a => [a] -> [a]
allSquares xs = map square xs where square x = x * x


allToUpper :: String -> String
allToUpper string = map toUpper string



type Point = (Float, Float)
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



-- Anonymous functions