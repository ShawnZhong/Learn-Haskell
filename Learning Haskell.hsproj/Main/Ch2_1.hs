module Ch2_1 where       -- Module definition
  
import Prelude hiding (max, signum) -- Hide default max and signum from Prelude

arithmetic_mean :: Fractional a => a -> a -> a
arithmetic_mean x y  = (x + y)  / 2

harmonic_mean :: Fractional a => a -> a -> a
harmonic_mean x y  = 2 * x * y / (x + y)


max :: Ord a => a -> a -> a
--max x y = if x >= y then x else y -- Defined using control flow
max x y | x >= y     = x            -- Defined using guards
        | otherwise  = y            -- otherwise == True

signum :: (Ord a, Num a) => a -> Int
--signum x = if x < 0 then -1 else if x == 0 then 0 else 1
signum x | x <  0     = -1
         | x == 0     = 0
         | otherwise  = 1


pi :: Floating a => a               -- Constant binding
pi = 3.141592653589793              -- Used as Ch2.pi


circleArea :: Floating a => a -> a
circleArea radius     = Ch2_1.pi * radius * radius

circleArea' :: Floating a => a -> a
circleArea' diameter  = Ch2_1.pi * radius * radius
  where
    radius = diameter / 2.0         -- Local binding
    

addMul :: Num a => a -> a -> (a, a)
addMul x y = (x + y, x * y)         -- Return a tuple

means :: Fractional a => a -> a -> (a, a)
means x y = (arithmetic_mean x y, harmonic_mean x y)


type Point = (Int, Int)             -- Declare a new type

origin :: Point
origin  = (0, 0)
    
moveRight :: Point -> Int -> Point
moveRight (x, y) distance  = (x + distance, y)

moveUp :: Point -> Int -> Point
moveUp (x, y) distance  = (x, y + distance)


type Colour = String
type ColourPoint = (Int, Int, Colour)

move :: ColourPoint -> Int -> Int -> ColourPoint
move (x, y, colour) xDistance yDistance  
  = (x + xDistance, y + yDistance, colour)

colourOfPoint (x, y, colour) = colour 