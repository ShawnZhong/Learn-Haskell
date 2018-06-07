module Ch6 where

import Graphics.Rasterific hiding (Point, Vector, Line, Path)
import Graphics.Rasterific.Texture
import Graphics.Rasterific.Transformations
import Codec.Picture

-- Create a new data type called `LineStyle`
-- Solid, Dashed, Dotted are called the data constructors
-- Data constructors must start with an uppercase letter
data LineStyle = Solid | Dashed | Dotted deriving (Show, Eq)

type Line  = (Point, Point)
type FancyLine = ((Float, Float), (Float, Float), LineStyle)

myLine :: FancyLine
myLine = ((0, 0), (1, 1), Dashed)

changeLineStyle :: FancyLine -> LineStyle -> FancyLine
changeLineStyle (x, y, _) newStyle = (x, y, newStyle)








-- Enumeration Types
data Day =
  Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
  deriving (Enum, Show, Eq, Ord)
-- include the new type Day into the standard type class Enum
-- [Monday .. Friday] == [Monday, Tuesday, Wednesday, Thursday, Friday]







-- Pattern matching and case expressions
isWeekday :: Day -> Bool
isWeekday Sunday   = False
isWeekday Saturday = False
isWeekday _        = True --If no pattern matches, evaluation raises a exception

-- Equivalent Syntaxs
isWeekday' :: Day -> Bool
isWeekday' day = case day of
                  Sunday   -> False
                  Saturday -> False
                  _        -> True
                  

isWeekday'' :: Day -> Bool
isWeekday'' day = case day of {Sunday -> False ; Saturday -> False; _ -> True}


isWeekend :: Day -> Bool
isWeekend day = day `elem` [Saturday, Sunday]











-- Product Types as Parametrised Data Constructors
data Point = Point Float Float deriving (Show, Eq)
-- Note: The second Point is a data constructor, Point :: Float -> Float -> Point
-- Data Type Definition: data <Type> = <Constructor> <Type1> ⋯ <TypeN> deriving <Classes>
-- Type of the data constructor: <Constructor> :: <Type1> -> ⋯ -> <TypeN> -> <Type>

data Vector = Vector Float Float deriving (Show, Eq)

zeroPoint :: Point
zeroPoint = Point 0 0

pointToVector :: Point -> Vector
pointToVector (Point x y) = Vector x y

movePointN :: Float -> Vector -> Point -> Point
movePointN n (Vector vx vy) (Point x y) = Point (n * vx + x) (n * vy + y)


data Colour = Colour Int Int Int Int deriving (Show, Eq)

white, black, blue, red, green, orange, magenta :: Colour
white      = Colour 255  255 255 255
black      = Colour   0    0   0 255
blue       = Colour   0    0 255 255
red        = Colour 255    0   0 255
green      = Colour  10  255  10 235
yellow     = Colour  255 255   0 235
magenta    = Colour 153    0 153 255
orange     = Colour 254  154  46 255










-- Sum Types as Alternative Data Constructors

data FillStyle = NoFill | SolidFill deriving (Eq, Show)

data PictureObject 
  = Path    [Point]                   Colour LineStyle 
  | Circle  Point   Float             Colour LineStyle FillStyle 
  | Ellipse Point   Float Float Float Colour LineStyle FillStyle 
  | Polygon [Point]                   Colour LineStyle FillStyle
  deriving (Show, Eq)

myPath :: PictureObject
myPath = Path [Point 210 200, Point 270 200, Point 545 600,
               Point 525 600, Point 380 390, Point 250 600,
               Point 230 600, Point 370 380, Point 260 215,
               Point 210 215] red Solid


dashedCircle, dottedCircle, solidCircle :: PictureObject
dashedCircle = Circle (Point 400 400) 180 blue  Dashed NoFill
dottedCircle = Circle (Point 400 400)  90 green Dotted NoFill
solidCircle  = Circle (Point 400 400)  20 red   Solid  SolidFill


redEllipse, greenEllipse, blueEllipse :: PictureObject
redEllipse   = Ellipse (Point 400 400) 300 100      0 red   Solid SolidFill
greenEllipse = Ellipse (Point 400 400) 300 100 (pi/4) green Solid SolidFill
blueEllipse  = Ellipse (Point 400 400) 300 100 (pi/2) blue  Solid SolidFill










-- Generating pictures
type Picture = [PictureObject]

simpleEllipsePic :: Float -> Picture
simpleEllipsePic n = map (\angle -> Ellipse (Point 400 400) 250 70 angle myGreen Dashed SolidFill)
      [0, pi/n..(n-1) * pi/n]
  where
    myGreen = Colour 27 230 34 80



















-- drawPicture Function
drawPicture linewidth picture 
  = renderDrawing  800 800 (toColour (Colour 0 0 0 255)) $ do
      { mapM drawObj picture
      ; return ()
      }
  where
    style SolidFill _ = fill
    style _ Solid     = stroke linewidth  JoinRound (CapRound, CapRound)  
    style _ Dashed    = dashed linewidth  JoinRound (CapRound, CapRound) 
    style _ Dotted    = dotted linewidth  JoinRound (CapRound, CapRound) 

    dotted = dashedStroke [linewidth/12, 2 * linewidth]
    dashed = dashedStroke [3* linewidth, 6 * linewidth] 

    texture colour = withTexture (uniformTexture $ toColour colour) 
    textureG  (x1, y1) (x2, y2) 
      = withTexture (linearGradientTexture  
          [(0, PixelRGBA8 255 0 0 255), (1, PixelRGBA8 255 255 255 255)] 
                (V2 x1 y1)(V2 x2 y2))   
    drawObj (Path points colour lineStyle) =
      texture colour
         $ style NoFill lineStyle
         $ polyline 
         $ map (\((Point x y)) -> V2 x y) points
    drawObj (Circle (Point px py) radius colour lineStyle fillStyle) =
      texture colour
         $ style fillStyle lineStyle
         $ circle (V2 px py) radius
    drawObj (Ellipse (Point px py) h w r colour lineStyle fillStyle) =
      texture colour
         $ style fillStyle lineStyle 
           . transform (applyTransformation 
                       $ rotateCenter r (V2 px py))
         $ ellipse (V2 px py) h w
    drawObj (Polygon points colour lineStyle fillStyle) =
      texture colour
         $ style fillStyle lineStyle
         $ polygon 
         $ map (\((Point x y)) -> V2 x y) points
           

    toColour (Colour a b c d) 
      = PixelRGBA8 (fromIntegral a) (fromIntegral b) (fromIntegral c) (fromIntegral d)