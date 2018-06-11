module Ch3_3 where
  
import Data.Char

-- Left Reduce
-- Subtract a list of numbers from a initial value
deductFromAccount :: Int -> [Int] -> Int
deductFromAccount balance []       = balance
deductFromAccount balance (d : ds) = deductFromAccount (balance - d) ds



-- String to Int
stringToInt :: String -> Int
stringToInt str = stringToIntAcc 0 str where
  stringToIntAcc :: Int -> String -> Int
  stringToIntAcc acc [] = acc
  stringToIntAcc acc (chr : restString) = 
      stringToIntAcc (10 * acc + digitToInt chr) restString



-- Fast reverse using left association
fastReverse :: [a] -> [a]
fastReverse xs = reverseAcc [] xs where
  reverseAcc list [] = list
  reverseAcc list (x : xs) = reverseAcc (x : list) xs




-- Sum from right to left, more efficient by using a constant amount of call stack memory
fastSum :: Num a => [a] -> a
fastSum xs = sumAcc 0 xs where
  sumAcc :: Num a => a -> [a] -> a
  sumAcc acc [] = acc
  sumAcc acc (x : xs) = sumAcc (acc + x) xs



-- Two versions of sumEvenElems
sumEvenElems :: Integral a => [a] -> a
sumEvenElems xs = sum (filterEven xs) where
  filterEven [] = []
  filterEven (x : xs)
    | even x    = x : filterEven xs
    | otherwise = filterEven xs
sumEvenElems' :: Integral a => [a] -> a
sumEvenElems' [] = 0
sumEvenElems' (x : xs)
  | even x    = x + sumEvenElems' xs
  | otherwise = sumEvenElems' xs



-- Two versions of sumOfSquareRoots
sumOfSquareRoots xs = sum (allSquareRoots (filterPositives xs)) where
allSquareRoots []     = []
allSquareRoots (x:xs) = sqrt x : allSquareRoots xs
filterPositives []    = []
filterPositives (x:xs)
  | x > 0     = x : filterPositives xs
  | otherwise = filterPositives xs 
sumOfSquareRoots' [] = 0
sumOfSquareRoots' (x:xs)
  | x > 0     = sqrt x + sumOfSquareRoots' xs
  | otherwise = sumOfSquareRoots' xs   



-- Calculate the closest point to a given point
type Point = (Float, Float)

distance :: Point -> Point -> Float
distance (x1, y1) (x2, y2) = sqrt(dx * dx + dy * dy) where
  dx = x1 - x2
  dy = y1 - y2
closestOfTwo :: Point -> Point -> Point -> Point
closestOfTwo p p1 p2
    | distance p p1 < distance p p2 = p1
    | otherwise                     = p2 
closestPoint :: Point -> [Point] -> Point
closestPoint p0 [p] = p
closestPoint p0 (p: ps) = closestOfTwo p0 p (closestPoint p0 ps)