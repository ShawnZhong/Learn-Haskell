module Ch6_4 where

import ShapeGraphics


rotatePoint :: Float -> Point -> Point -> Point
rotatePoint alpha (Point x0 y0) (Point x y)
  = Point (cos alpha * nx - sin alpha * ny + x0)
          (sin alpha * nx + cos alpha * ny + y0)
  where
    nx = x - x0
    ny = y - y0

rotatePictureObject :: Float -> Point -> PictureObject -> PictureObject
rotatePictureObject angle point picObj@Circle{}
  = picObj{ centerPO = rotatePoint angle point $ centerPO picObj }
rotatePictureObject angle point picObj@Ellipse{}
  = picObj{ centerPO   = rotatePoint angle point $ centerPO picObj
          , rotationPO = rotationPO picObj + angle
          }
rotatePictureObject angle point picObj
  = picObj{ pointsPO = map (rotatePoint angle point) $ pointsPO picObj }
  



rotatePic :: Int -> PictureObject -> Picture
rotatePic n shape = map (\n -> rotatePictureObject (0.2 * fromIntegral n) (Point 400 400) shape) [0..n]




dynamicRotate:: PictureObject -> Point -> Vector -> Float -> Int -> Picture
dynamicRotate picObj _   _   _     0 = [picObj]
dynamicRotate picObj pnt vec alpha n 
  = rotatePicObj : dynamicRotate (movePictureObject vec rotatePicObj)(movePoint vec pnt) vec alpha (n-1)
  where
    rotatePicObj = rotatePictureObject alpha pnt picObj








-- Additional Functions
movePoint :: Vector -> Point -> Point
movePoint (Vector vx vy) (Point x y) = Point (vx + x) (vy + y)

movePictureObject :: Vector -> PictureObject -> PictureObject
movePictureObject vec picObj@Path{} -- Use {} if no fields needed
  = picObj{pointsPO = map (movePoint vec) $ pointsPO picObj}
movePictureObject vec picObj@Polygon{} 
  = picObj{pointsPO = map (movePoint vec) $ pointsPO picObj}
movePictureObject vec picObj   -- For Circle and Ellipse
  = picObj{centerPO = movePoint vec $ centerPO picObj}
