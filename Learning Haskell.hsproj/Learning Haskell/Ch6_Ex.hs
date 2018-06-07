{-# LANGUAGE NamedFieldPuns #-}
module Ch6_Ex where
  
import Prelude hiding (map)
import Ch6_1 (Day(..))
import ShapeGraphics

-- Exercise 1
map :: (a -> b) -> [a] -> [b]
map f xs = case xs of 
          [] -> []
          (x:xss) -> f x : map f xss
          


-- Exercise 2
nextDay :: Day -> Day
nextDay day = succ day



-- Exercise 3
data Suit = Diamonds | Clubs | Hearts | Spades
data Card = Card {suit :: Suit, number::Int}
type Hand = [Card]
cardValue :: Card -> Int
cardValue Card {number} 
  | number >= 10 = 10
  | number == 1  = 11
  | otherwise    = number
calculateHandValue :: Hand -> Int
calculateHandValue hands 
  | tmpSum > 21 = tmpSum - 10
  | otherwise   = tmpSum where
    tmpSum = sum $ map cardValue hands
    

-- Exercise 4
simpleCirclePic :: Float -> Picture
simpleCirclePic n = map (\r -> Circle (Point 400 400) r myGreen Dashed SolidFill)
                        [0,n..400] 
                        where myGreen = Colour 27 230 34 80
    



-- Exercise 5
triangle :: Point -> Point -> Point -> PictureObject
triangle vA vB vC = Polygon [vA, vB, vC] red Solid SolidFill

sierpinski :: Int -> Picture
sierpinski 1 = [triangle (Point 400 0) (Point 0 692) (Point 800 692)]
sierpinski n = concat $ map splitToThree $ sierpinski (n - 1) where
    splitToThree :: PictureObject -> [PictureObject]
    splitToThree tri = 
      triangle vA (midPoint vA vB) (midPoint vA vC)
      : triangle vB (midPoint vB vA) (midPoint vB vC)
      : triangle vC (midPoint vC vA) (midPoint vC vB) : [] where
        
        
        vA, vB, vC :: Point
        vA = pointsPO tri !! 0
        vB = pointsPO tri !! 1
        vC = pointsPO tri !! 2
        
        midPoint :: Point -> Point -> Point
        midPoint (Point x1 y1) (Point x2 y2) = Point ((x1+x2)/2) ((y1+y2)/2)
    


