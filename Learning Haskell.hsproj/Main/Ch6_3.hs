module Ch6_3 where

import ShapeGraphics (LineStyle, FillStyle, Picture)

-- Records
data Colour
  = Colour { redC      :: Int
           , greenC    :: Int
           , blueC     :: Int
           , opacityC  :: Int
           }
  deriving (Show, Eq)
  

-- Use names to clarify the meaning of each argument:
red :: Colour
red = Colour{redC = 255, opacityC = 255, blueC = 0, greenC = 0}


-- Use patten matching for argument
greenComponent :: Colour -> Int
greenComponent Colour{greenC = green} = green

-- Note that in the definition of Colour, following functions are defined
-- redC     :: Colour -> Int
-- greenC   :: Colour -> Int
-- blueC    :: Colour -> Int
-- opacityC :: Colour -> Int
greenComponent' :: Colour -> Int
greenComponent' = greenC

isOpaqueColour :: Colour -> Bool
isOpaqueColour colour = opacityC colour == 255





-- Projection functions and field names
data Point = Point { xPoint :: Float, yPoint :: Float} deriving (Show, Eq)
data Vector = Vector { xVector :: Float, yVector :: Float} deriving (Show, Eq)
-- Note: We cannot use x for both xPoint and xVector 
movePointN :: Float -> Vector -> Point -> Point
movePointN n vector point
  = Point { xPoint = n * xVector vector + xPoint point 
          , yPoint = n * yVector vector + yPoint point
          }
movePoint :: Vector -> Point -> Point
movePoint = movePointN 1
          





-- Record updates
-- <Expr> { <FieldName1> = <Expr1>, … <FieldNameN> = <ExprN> }
fade :: Colour -> Colour
fade colour = colour{opacityC = max 0 (opacityC colour - 10)}


setLineStyle :: PictureObject -> LineStyle -> PictureObject
setLineStyle (Path points colour _) newLineStyle
  = Path points colour newLineStyle
  

data PictureObject 
  = Path    
    { pointsPO    :: [Point] 
    , colourPO    :: Colour
    , lineStylePO :: LineStyle
    }
  | Circle  
    { centerPO    :: Point
    , radiusPO    :: Float
    , colourPO    :: Colour
    , lineStylePO :: LineStyle
    , fillStylePO :: FillStyle 
    }
  | Ellipse 
    { centerPO    :: Point
    , widthPO     :: Float
    , heightPO    :: Float
    , rotationPO  :: Float
    , colourPO    :: Colour
    , lineStylePO :: LineStyle
    , fillStylePO :: FillStyle
    }
  | Polygon 
    { pointsPO    :: [Point]
    , colourPO    :: Colour
    , lineStylePO :: LineStyle
    , fillStylePO :: FillStyle 
    }
-- Note: lineStylePO :: PictureObject -> LineStyle is used for all data constructors

setLineStyle' :: PictureObject -> LineStyle -> PictureObject
setLineStyle' picObj newLineStyle = picObj{lineStylePO = newLineStyle}


movePictureObject :: Vector -> PictureObject -> PictureObject
movePictureObject vec picObj@Path{} -- Use {} if no fields needed
  = picObj{pointsPO = map (movePoint vec) $ pointsPO picObj}
movePictureObject vec picObj@Polygon{} 
  = picObj{pointsPO = map (movePoint vec) $ pointsPO picObj}
movePictureObject vec picObj   -- For Circle and Ellipse
  = picObj{centerPO = movePoint vec $ centerPO picObj}
  








