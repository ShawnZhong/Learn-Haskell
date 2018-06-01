module HS1_Fi1rst_Steps where

inc :: Num a => a -> a      -- type signature
inc x = x + 1               -- function equation

exclaim :: String -> String
exclaim sentence = sentence ++ "!"

average :: Float -> Float -> Float      -- Legal, right association of function definition
--average :: Float -> (Float -> Float)  -- Legal, curried function
--average :: (Float -> Float) -> Float  -- Illegal
averagePair :: (Float, Float) -> Float  -- Legal, but different sigunature

average      a  b  = (a + b) / 2
averagePair (a, b) = (a + b) / 2

