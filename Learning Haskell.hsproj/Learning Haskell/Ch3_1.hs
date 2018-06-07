module Ch3_1 where
  
import Data.Char

-- Compute the sum from 0 to n
natSum :: (Num a, Ord a) => a -> a
natSum 0 = 0
natSum n  | n > 0     = n + natSum (n - 1) 
          | otherwise = error "natSum: Invalid Input!"  


-- Create a List of the same element x
repeatN :: Int -> a -> [a]
repeatN 0 x  = []
repeatN n x  = x : repeatN (n - 1) x

-- Create a suffixes list
suffixes :: [a] -> [[a]]
suffixes []  = []
suffixes x = x : suffixes (tail x)







-- Mapping: applying an operation to every element of a list
-- Square each element in a list
allSquares :: Num a => [a] -> [a]
allSquares [] = []
allSquares (x : xs) = x * x : allSquares xs


-- Convert to upper case
allToUpper :: String -> String
allToUpper []       = []
allToUpper (x : xs) = toUpper x : allToUpper xs


-- Calculate the distance between a given point and a list of points
type Point = (Int, Int)
distance :: Point -> Point -> Float
distance (x1, y1) (x2, y2) = sqrt(fromIntegral(x1 - x2) ^ 2 + fromIntegral(y1 - y2) ^ 2)

distancesFromPoint :: Point -> [Point] -> [Float]
distancesFromPoint point [] = []
distancesFromPoint point (p : ps) = distance point p : distancesFromPoint point ps









-- Filtering: removing elements from a list
-- Extract digits from a string
extractDigits :: String -> String
extractDigits []  = []
extractDigits (x : xs)
  | isDigit   x = x : extractDigits xs
  | otherwise   = extractDigits xs