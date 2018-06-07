module Ch4_Ex where

import LineGraphics
import Ch4

colouredFTree :: Float -> Int -> Colour -> Line -> Picture
colouredFTree factor n color line = [(newColour, fractalTree factor n newLine)] where
   newColour = if (n `mod` 3 == 0) then fade color else color
   newLine   = scaleLine 1.2 line