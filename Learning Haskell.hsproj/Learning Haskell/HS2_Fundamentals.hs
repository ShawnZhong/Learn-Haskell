module HS2_Fundamentals where

sort :: Ord a => a -> a -> (a, a)
sort x y | x <= y = (x,y)
         | otherwise = (y,x)
         
almostEqual :: Eq a => (a, a) -> (a, a) -> Bool
almostEqual (x1,y1) (x2, y2) 
  | x1==x2 && y1 == y2 || x1==y2 && x2 == y1 = True 
  | otherwise = False
  
isLower :: Char -> Bool
isLower x | elem x ['a'..'z'] = True | otherwise = False