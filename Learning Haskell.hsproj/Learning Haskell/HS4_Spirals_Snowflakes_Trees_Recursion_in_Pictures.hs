module HS4_Spirals_Snowflakes_Trees_Recursion_in_Pictures where 

-- needed to display the picture in the playground
import Codec.Picture

-- our line graphics programming interface
import LineGraphics



-- Drawing spiral rays 
rotateLine :: Float -> Line -> Line
rotateLine alpha ((x1, y1), (x2, y2)) = ((x1, y1), (x' + x1, y' + y1)) where
  x0 = x2 - x1
  y0 = y2 - y1
  x' = x0 * cos alpha - y0 * sin alpha
  y' = x0 * sin alpha + y0 * cos alpha
      
scaleLine :: Float -> Line -> Line
scaleLine factor ((x1, y1), (x2, y2)) = ((x1, y1), (x' + x1, y' + y1)) where
  x0 = x2 - x1
  y0 = y2 - y1
  x' = factor * x0 
  y' = factor * y0
      

fade :: Colour -> Colour
fade (redC, greenC, blueC, opacityC) = (redC, greenC, blueC, opacityC - 1)


spiralRays :: Float -> Float -> Int -> Colour -> Line -> Picture
spiralRays angle scaleFactor n colour line  = spiralRays' n colour line where
  spiralRays' n colour line@(p1, p2)
    | n <= 0 = []
    | otherwise = (colour, [p1, p2]) : spiralRays' (n-1) newColour newLine where
      newColour = if (n `mod` 3 == 0) then fade colour else colour
      newLine   = scaleLine scaleFactor (rotateLine angle line)
      




-- Drawing spirals
connectLine :: Line -> Line -> Line
connectLine line1@(_, p2) line2 = startLineFrom p2 line2 where
  startLineFrom :: Point -> Line -> Line
  startLineFrom startPoint@(x0, y0) ((xS, yS), (xE, yE)) 
     = (startPoint, ((x0 + xE - xS, y0 + yE - yS))) 

spiral :: Float -> Float -> Int -> Line -> Path
spiral angle scaleFactor n line = spiral' n line where
    spiral' n line@(p1, p2)
      | n <= 0    = []
      | otherwise = p1 : spiral' (n - 1) newLine
      where
        newLine = connectLine line (scaleLine scaleFactor (rotateLine angle line))
        


-- Drawing polygons
polygon :: Int -> Line -> Path
polygon n line | n > 2 = spiral rotationAngle 1 (n + 1) line where 
    rotationAngle = (2 * pi) / (fromIntegral n)
    



-- Drawing Koch snowflakes
kochLine :: Int -> Point -> Point -> Path
kochLine n pS pE
  | n <= 0 = []
  | otherwise = [pS] ++ kochLine (n - 1) pS p1 
                     ++ kochLine (n - 1) p1 p2
                     ++ kochLine (n - 1) p2 p3 
                     ++ kochLine (n - 1) p3 pE          
                     ++ [pE] where 
    l1@(_, p1) = scaleLine (1 / 3) (pS, pE)
    l2@(_, p3) = connectLine l1 l1 
    (_, p2)    = rotateLine (5 / 3 * pi) $ l2

kochFlake :: Int -> Line -> Path
kochFlake n line = kochLine n p1 p2 ++ kochLine n p2 p3 ++ kochLine n p3 p1 where
  [p1, p2, p3, _] = polygon 3 line
  



-- Drawing Pythagorean Trees
fractalTree :: Float -> Int -> Line -> Path
fractalTree factor n line = fractalTree' n line where
    fractalTree' 0 line = []  
    fractalTree' n line = [p1, p4] ++ fractalTree' (n-1) (p4, p5)
                                   ++ fractalTree' (n-1) (p5, p3)
                                   ++  [p3, p2] where 
        -- flip direction of line
        flipLine :: Line -> Line
        flipLine (pS, pE) = (pE, pS)
        
        factor' = if (n `mod` 2 == 0) then factor else (1 - factor)
        
        [p1,p2,p3,p4,_] = polygon 4 line
        r               = flipLine (scaleLine 0.5 (p3, p4))
        (_, p5)         = rotateLine (factor' * pi) r 