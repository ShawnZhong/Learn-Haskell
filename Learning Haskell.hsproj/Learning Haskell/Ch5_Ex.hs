module Ch5_Ex where
  
import Prelude hiding (map)
import LineGraphics
import Ch4 hiding (fade, spiralRays)

-- Exercises 1
natSum :: (Num a, Enum a) => a -> a
natSum n = sum $ enumFromTo 1 n


-- Exercises 2
map :: (a -> b) -> [a] -> [b]
map f = foldr (\a bs -> (f a) : bs) []


-- Exercies 3
--foldr (:) [] does nothing to a list


-- Exercies 4
spiralRays :: Int -> (Int -> Colour) -> Line -> Picture
spiralRays n colourFun line@(p1, p2)
  | n <= 0    = []
  | otherwise = (colourFun n, [p1, p2]) : spiralRays (n - 1) colourFun newLine
  where
   newLine   = scaleLine 1.02 (rotateLine (pi / 40) line)
   


-- Exercies 5
spiralRays' :: Int -> (Int -> Colour) -> Line -> Picture
spiralRays' n colourFun line = map spiralFun $ [1..n]
  where
   spiralFun :: Int -> (Colour, Path)
   spiralFun n = (colourFun n, [p1, p2]) where
     newLine@(p1, p2)   = scaleLine (1.02 ^ n) (rotateLine (pi / 40 * (fromIntegral n)) line)