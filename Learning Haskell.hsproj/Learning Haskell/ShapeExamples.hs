module ShapeExamples where
  
import Codec.Picture
import ShapeGraphics


-- operations to move points & picture objects

movePoint :: Vector -> Point -> Point
movePoint (Vector xv yv) (Point xp yp)
 = Point (xv + xp) (yv + yp)

movePictureObject :: Vector -> PictureObject -> PictureObject
movePictureObject vec picObj@(Path _ _ _) 
  = picObj {pointsPO = map (movePoint vec) $ pointsPO picObj}
movePictureObject vec picObj@(Polygon _ _ _ _) 
  = picObj {pointsPO = map (movePoint vec) $ pointsPO picObj}
movePictureObject vec picObj
  = picObj {centerPO = movePoint vec $ centerPO picObj}

-- operations to rotate point & picture objects

rotatePoint :: Float -> Point -> Point -> Point
rotatePoint alpha (Point x1 y1) (Point x2 y2)
  = Point (cos alpha * nx - sin alpha * ny + x1)
          (sin alpha * nx + cos alpha * ny + y1)
  where
    nx = x2 - x1
    ny = y2 - y1

rotatePictureObject :: Float -> Point -> PictureObject -> PictureObject
rotatePictureObject angle point pic@(Circle _ _ _ _ _)
  = pic {centerPO = rotatePoint angle point $ centerPO pic}
rotatePictureObject angle point pic@(Ellipse _ _ _ _ _ _ _)
  = pic {centerPO = rotatePoint angle point $ centerPO pic, rotationPO = rotationPO pic + angle}
rotatePictureObject angle point pic
  = pic {pointsPO = map (rotatePoint angle point) $ pointsPO pic}


-- operations to scale line and picture object size

scaleLine :: Float -> Line -> Line
scaleLine f (Line (Point x1 y1) (Point x2 y2))
  = Line  (Point x1 y1) 
          (Point (x1 + (x2 -x1) * f)  (y1 + (y2 - y1) * f))

scaleObject :: Float  -> PictureObject -> PictureObject
scaleObject factor picObj 
  = error "Implement me!"
  


-- lambda path
myPath :: PictureObject
myPath = Path [Point 210 200, Point 270 200, Point 545 600,
               Point 525 600, Point 380 390, Point 250 600,
               Point 230 600, Point 370 380, Point 260 215,
               Point 210 215]  red Solid

dashedCircle, dottedCircle, solidCircle :: PictureObject
dashedCircle = Circle (Point 400 400) 180 blue    Dashed NoFill
dottedCircle = Circle (Point 400 400)  90 green   Dotted NoFill
solidCircle  = Circle (Point 400 400)  20 red     Solid  SolidFill


redEllipse, greenEllipse, blueEllipse :: PictureObject
redEllipse
  = Ellipse (Point 400 400) 300 100      0 red   Solid SolidFill
greenEllipse
  = Ellipse (Point 400 400) 300 100 (pi/4) green Solid SolidFill
blueEllipse
  = Ellipse (Point 400 400) 300 100 (pi/2) blue Solid SolidFill

simpleEllipsePic :: Float -> Picture
simpleEllipsePic n
  = map makeGreenEllipse [0, pi/n..(n-1) * pi/n]
  where
    makeGreenEllipse angle = Ellipse (Point 400 400) 250 70 angle myGreen Dashed SolidFill
    myGreen                = Colour 0 255 0 80


simplePolyPic :: Float -> Picture 
simplePolyPic n 
  = map makePoly [0,n..400]
  where
    grey = Colour 255 255 255 45
    makePoly n = Polygon [Point 0 800, Point (400 + n) (400 + n), 
                          Point 800 0, Point (400 - n) (400 - n)] 
                          grey Solid SolidFill


simpleCirclePic :: Float -> Picture 
simpleCirclePic n  = map makeCircle [0,n..400]
  where
    grey = Colour 200 55 155 65
    makeCircle n = Circle (Point 400 400) n  
                          grey Solid SolidFill




isOpaqueColour :: Colour -> Bool
isOpaqueColour (Colour _ _ _ opC)
  = opC == 255
  




dynamicRotate:: PictureObject -> Point -> Vector -> Float -> Int -> Picture
dynamicRotate po _ _ _ 0 = [po]
dynamicRotate po pnt vec alpha n 
  = rpo : dynamicRotate (movePictureObject vec rpo)(movePoint vec pnt) vec alpha (n-1)
  where
    rpo = rotatePictureObject alpha pnt po 


curves :: Picture
curves
  = map makeCircleSin  xvals ++
    map makePolygonCos xvals ++
    map makeEllipseSin xvals ++
    map makePathCos    xvals
    where
      redCircle      = Circle  (Point 20 380) 10 red Solid SolidFill
      greenEllipse   = Ellipse (Point 20 380) 5 30 0 green Solid SolidFill
      whitePolygon   = Polygon [Point 20 380, Point 40 380, Point 40 420] white Solid SolidFill
      bluePath       = Path    [Point 20 380, Point 40 380, Point 40 420] blue Solid 
      xvals  = [0,10..780]
      makeCircleSin  x = movePictureObject (Vector x $ 100 * sin (pi * x/200)) redCircle
      makePolygonCos x = movePictureObject (Vector x $ 100 * cos (pi * x/200)) whitePolygon
      makeEllipseSin x = movePictureObject (Vector x $ 250 * sin (pi * x/200)) greenEllipse
      makePathCos    x = movePictureObject (Vector x $ 200 * cos (pi * x/200)) bluePath

-- solutions for exercises

rotate90 :: Line -> Point
rotate90 (Line (Point x1 y1) (Point x2 y2))
  = Point (-1 * ny + x1)
          (nx + y1)
  where
    nx = x2 - x1
    ny = y2 - y1

rotate270 :: Line -> Point
rotate270 (Line (Point x1 y1) (Point x2 y2))
  = Point (ny + x1)
          (-1 * nx + y1)
  where
    nx = x2 - x1
    ny = y2 - y1


sierpinsky :: Int -> Line -> Picture
sierpinsky 0 (Line p1 p2)
  = [Polygon [p1, p2,
             rotatePoint (5*pi/3) p1 p2] 
             red Solid SolidFill]
sierpinsky n (Line s@(Point x1 y1) e@(Point x2 y2))
  = sierpinsky (n-1) (Line s (Point midx midy)) ++
    sierpinsky (n-1) (Line (Point midx midy) e) ++
    sierpinsky (n-1) (Line p1 p2)
  where
    midx = x1 + (x2 -x1)/2
    midy = y1 + (y2 -y1)/2
    p1   = rotatePoint (5*pi/3) s (Point midx midy)
    p2   = rotatePoint (5*pi/3) (Point midx midy) e
    

colouredFTree' :: (Int -> Float) -> Int -> (Int -> Colour) -> Line -> Picture
colouredFTree' factorFun n colourFun line = fT n line 
  where
    fT n line = 
      if (n == 0) 
       then  [Polygon [p1, p2, p3, p5, p4] (colourFun n) Solid SolidFill]  
       else  [Polygon [p1, p2, p3, p5, p4] (colourFun n) Solid SolidFill]  
                       ++ fT (n-1) (Line p5 p3) 
                       ++ fT (n-1) (Line p4 p5)
      where 
        [p1,p2,p3,p4] = rectangle line (8.5 - (fromIntegral n)*0.46)
                         --equiPolygon 4 line
        p5          = (\(Line x y) -> rotatePoint ((factorFun n) * pi) y x )
                            $ scaleLine 0.36 (Line p3 p4)


        rectangle line@(Line p1 p2) factor = [p1, p2, p3, p4]
            where
              p4 = rotate90 (scaleLine factor line)
              p3 = rotate270 (scaleLine factor (Line p2 p1))
