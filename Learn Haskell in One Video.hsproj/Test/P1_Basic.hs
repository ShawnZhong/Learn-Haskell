module P1_Basic where
  
-- Comments
{- 
  Comments
-}





-- Data Types
-- Int: -2^63 ~ 2^63 - 1
maxInt = maxBound :: Int
minInt = minBound :: Int
always5 :: Int
always5 = 5
-- Integer: not bounded
integer = 2^1024 :: Integer
-- Floats & Doubles
bigFloat = 3.99999 + 0.000005 :: Float
bigDouble = 3.999999999999 + 0.0000000000005 :: Double
-- Bool
-- Char
-- Tuple






-- Math
sumOfNums = sum [1..1000]

addEx = 5 + 4                       -- Num a => a -> a -> a
subEx = 5 - 4
multEx = 5 * 4
divEx = 5 / 4                       -- get 1.25 instead of 1
modEx = 5 `mod` 4
modEx' = mod 5 4
negNumEx = 5 + (-4)                 -- must add () for negative number

num9 = 9 :: Int
sqrtOf9 = sqrt (fromIntegral num9)  -- sqrt :: Floating a => a -> a
                                    -- fromIntegral :: (Integral a, Num b) => a -> b
piVal = pi
ePow9 = exp 9
logOf9 = log 9
squared9 = 9 ** 2
truncateVal = truncate 9.999        -- the integer nearest x between zero and x
roundVal = round 9.999
ceilinVal = ceiling 9.999
floorVal = floor 9.999              -- the greatest integer not greater than x
-- sin, cos, tan, asin, atan, acos, sinh, tanh, asinh, atanh, acosh









-- Logic
trueAndFalse = True && False
trueOrFalse = True || False
notTrue = not(True)