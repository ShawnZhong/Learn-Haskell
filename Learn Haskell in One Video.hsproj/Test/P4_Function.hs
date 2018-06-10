module P4_Function where

main = do                         -- do chain a bunch of commands 
  putStrLn "What's you name"
  name <- getLine
  putStrLn ("Hello " ++ name)
  


-- Function definition
-- funcName :: type1 -> type2 -> return type
-- funcName param1 param2 = operations (returned value)

addMe :: Int -> Int -> Int
addMe x y = x + y

sumMe x y = x + y                 -- Haskell can infer the type declaration 
                                  -- sumMe :: Num a => a -> a -> a
                                  

addTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuples (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)



whatAge :: Int -> String
whatAge 16 = "You can drive"
whatAge 18 = "You can vote"
whatAge 21 = "You are an adult"
whatAge _  = "Nothing important"



factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)