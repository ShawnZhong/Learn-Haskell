module P5_Function where
  

-- Guard
isOdd :: Int -> Bool
isOdd n
    | n `mod` 2 == 0 = False    
    | otherwise      = True
    
isEven n = n `mod` 2 == 0


whatGrade age
  | (age >= 5) && (age <= 6)  = "Kindergarten"
  | (age > 6) && (age <= 10)  = "Elementary School"
  | (age > 10) && (age <= 14) = "Middle School"
  | (age > 14) && (age <= 18) = "High School"
  | otherwise                 = "Go to college"
  

-- Where
batAvgRating :: Double -> Double -> String
batAvgRating hits atBats
  | avg <= 0.200 = "Terrible Batting Average"
  | avg <= 0.250 = "Average Player"
  | avg <= 0.280 = "You are doing pretty good"
  | otherwise    = "You are a super star"
  where avg = hits / atBats
  

getListItems :: [Int] -> String
getListItems [] = "Your List is empty"
getListItems (x:[]) = "Your list starts with " ++ show x
getListItems (x:y:[]) = "Your list contians " ++ show x ++ " and " ++ show y
getListItems (x:xs) = "The 1st items is " ++ show x ++ "and the rest items are " ++ show xs


getFirstItem :: String -> String
getFirstItem [] = "Empty String"
getFirstItem all@(x:_) = "The first letter in " ++ all ++ " is " ++ [x]