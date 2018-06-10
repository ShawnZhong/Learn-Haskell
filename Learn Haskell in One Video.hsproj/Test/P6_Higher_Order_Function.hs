module P6_Higher_Order_Function where
  
-- map
times4 :: Int -> Int
times4 x = x * 4

listTimes4 = map times4 [1,2,3,4,5]



multBy4 :: [Int] -> [Int]
multBy4 [] = []
multBy4 (x:xs) = times4 x : multBy4 xs