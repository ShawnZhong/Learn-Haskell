module Ch7_1 where
  
import Prelude hiding (Nothing, Just, Maybe)


-- Parameterised Data Types
showFifthElement :: Show a => [a] -> String
showFifthElement xs
  = if length xs < 5
      then "there is no fifth element in this list"
      else "the fifth element of the list is: " ++ show (xs !! 4)

--data MaybeInt = Just Int | Nothing   -- See improved version below

--(!+!) :: [Int] -> Int -> MaybeInt
--[]      !+! _ = Nothing
--(x : _) !+! 0 = Just x
--(_ :xs) !+! n = xs !+! (n - 1)


showFifthElement' :: [Int] -> String
showFifthElement' xs
  = case xs !+! 4 of
      Nothing -> "there is no fifth element in this list"
      Just n  -> "the fifth element of the list is: " ++ show n
      

data Maybe a = Just a | Nothing    -- This is a a parametric version of MaybeInt
  

(!+!) :: [a] -> Int -> Maybe a
[]      !+! _ = Nothing
(x : _) !+! 0 = Just x
(_ :xs) !+! n = (!+!) xs (n - 1)


-- Note: data can be also written in GADTs
-- data Maybe a where
--    Just     :: a -> Maybe a
--    Nothing  ::      Maybe a
-- In this case, Just and Nothing are parametric polymorphic functions








-- List
data List a = Cons a (List a) | Nil

isElement :: Eq a => a -> [a] -> Bool
isElement _ []     = False
isElement a (x:xs)
  | a == x         = True
  | otherwise      = isElement a xs






--  Binary Trees