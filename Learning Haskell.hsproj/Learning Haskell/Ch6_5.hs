{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}


module Ch6_5 where
  
import ShapeGraphics (Colour(..), LineStyle, FillStyle)

-- Data Constructors Versus Functions
-- We may regard data constructors as a special kind of function
-- Data constructors are sometimes called free or uninterpreted functions
-- They merely store, but do not process their arguments.

data Point = Point{pointX :: Float, pointY:: Float} deriving (Eq)

zeroX :: Float -> Point
zeroX = Point 0








-- Generalised syntax for algebraic data types

data Point' where Point' :: Float -> Float -> Point'

data PictureObject where
  Path    :: [Point] ->                            Colour -> LineStyle              -> PictureObject
  Circle  :: Point   -> Float ->                   Colour -> LineStyle -> FillStyle -> PictureObject
  Ellipse :: Point   -> Float -> Float -> Float -> Colour -> LineStyle -> FillStyle -> PictureObject
  Polygon :: [Point] ->                            Colour -> LineStyle -> FillStyle -> PictureObject
  


-- Record Puns and Wildcards
greenComponent :: Colour -> Int
greenComponent Colour{greenC} = greenC   -- with punning

greenComponent' :: Colour -> Int
greenComponent' Colour{greenC = greenC} = greenC   -- without punning


red = Colour{redC, opacityC, blueC, greenC}
  where
    redC     = 255
    opacityC = 255
    blueC    = 0
    greenC   = 0
    

red' = Colour{..}             -- with wildcard
  where 
    redC     = 255
    opacityC = 255
    blueC    = 0
    greenC   = 0
    


greenComponent'' :: Colour -> Int
greenComponent'' Colour{..} = greenC   -- with wildcard punning



brightGreenOpacity :: Colour -> Int
brightGreenOpacity Colour{greenC = 255, ..} = opacityC
brightGreenOpacity _                       = error "green not saturated"








