module Ch5 where

import Data.Char
import LineGraphics
import Prelude hiding (map, zipWith, filter, foldr, sum, concat, reverse, foldl, ($), (.))
import Ch4 hiding (fade)


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



toBlue n = (255 - n * 6, 255 - n * 3, 255, 255)

bluePolygons :: Line -> Int -> Picture
bluePolygons line n = map (\n -> (toBlue n, polygon n line)) $ [3..n]




-- Higher-order Functions Provide Flexibility

fade :: Colour -> Colour
fade col@(redC, greenC, blueC, opacityC)
  | opacityC == 0 = col
  | otherwise     = (redC, greenC, blueC, opacityC - 20)

fadedFTree :: Int -> Float -> Colour -> Line -> Picture
fadedFTree n factor colour line = fT n colour line 
  where
    fT 0 colour line = [(colour, [fst line, snd line])]  
    fT n colour line = [(colour,[p2, p3])]  
                       ++ [(colour,[p4, p1])] 
                       ++ fT (n-1) colour' (p5, p3) 
                       ++ fT (n-1) colour' (p4, p5)
      where 
        colour'         = fade colour
        [p1,p2,p3,p4,_] = polygon 4 line
        (_, p5)         = rotateLine (factor * pi)
                            $ (\(x,y) -> (y,x)) 
                            $ scaleLine 0.5 (p3, p4)






bleach :: Colour -> Colour
bleach (redC, greenC, blueC, opacityC)
  = (min 255 (redC + 18), 
     min 255 (greenC + 18), 
     min 255 (blueC + 18),
     opacityC)

bleachedFTree :: Int -> Float -> Colour -> Line -> Picture
bleachedFTree n factor colour line = fT n colour line 
  where
    fT 0 colour line = [(colour, [fst line, snd line])]  
    fT n colour line = [(colour,[p2, p3])]  
                       ++ [(colour,[p4, p1])] 
                       ++ fT (n-1) colour' (p5, p3) 
                       ++ fT (n-1) colour' (p4, p5)
      where 
        colour'         = bleach colour
        [p1,p2,p3,p4,_] = polygon 4 line
        (_, p5)         = rotateLine (factor * pi)
                            $ (\(x, y) -> (y, x)) 
                            $ scaleLine 0.5 (p3, p4)






colouredFTree :: Int -> (Int -> Float) -> (Int -> Colour) -> Line -> Picture
colouredFTree n factorFun colourFun line = fT n line 
  where
    fT 0  line = [(colourFun 0, [fst line, snd line])]  
    fT n  line = [(colourFun n, [p2, p3])]  
                       ++ [(colourFun n,[p4, p1])] 
                       ++ fT (n-1) (p5, p3) 
                       ++ fT (n-1) (p4, p5)
      where 
        [p1,p2,p3,p4,_] = polygon 4 line
        (_, p5)         = rotateLine ((factorFun n) * pi)
                            $ (\(x, y) -> (y, x)) 
                            $ scaleLine 0.5 (p3, p4)

magentaToWhite, toBlue1, toBlue2 :: Int -> Colour
magentaToWhite n = (127 + (18 - n) * 7, (18 - n) * 15, 255, 255)
toBlue1 n        = (255 - (16 - n) * 13, 255 - (16 - n) * 13, 214, 255)
toBlue2 n        = (51 + n * 8, 255 + 14 * n, 255, 255)

toggleFactor2, toggleFactor5, shiftFactor :: Float -> Int -> Float
toggleFactor2 factor n = if (n `mod` 2) == 0 then factor else (1 - factor)
toggleFactor5 factor n = if (n `mod` 5) == 0 then factor else (1 - factor)
shiftFactor factor n   = factor + (fromIntegral (16 - n)) * 0.025

