module P6_Higher_Order_Function where
  
times4 :: Int -> Int
times4 x = x * 4
listTimes4 = map times4 [1,2,3,4,5]



multBy4 :: [Int] -> [Int]
multBy4 [] = []
multBy4 (x:xs) = times4 x : multBy4 xs




areStringsEq :: [Char] -> [Char] -> Bool
areStringsEq [] [] = True
areStringsEq (x:xs) (y:ys) = x == y && areStringsEq xs ys
areStringsEq  _ _ = False




doMult :: (Int -> Int) -> Int
doMult func = func 3
num3Times4 = doMult times4



getAddFunc :: Int -> (Int -> Int)
getAddFunc x y = x + y
adds3 = getAddFunc 3
num4Plus3 = adds3 4



threePlusList = map adds3 [1,2,3,4,5]



dbl1To10 = map (\x -> x * 2) [1..10]

-- Comparison operator: > < >= <= == /=
-- Logic operator: && || not

doubleEvenNumber y = if (y `mod` 2 /= 0) then y else y * 2
getClass n = case n of 
    5 -> "Go to Kindergarten"
    6 -> "Go to elementary school"
    _ -> "Go away"