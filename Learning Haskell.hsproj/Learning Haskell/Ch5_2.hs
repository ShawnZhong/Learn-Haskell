module Ch5_2 where

import Ch4 (polygon, rotateLine, scaleLine)
import LineGraphics

-- Function composition
bluePolygons :: Line -> Int -> Picture
bluePolygons line n = map (\n -> (toBlue n, polygon n line)) $ [3..n] where
  toBlue n = (255 - n * 6, 255 - n * 3, 255, 255)




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
