module P8_Type_Classes where
  
-- Num Eq Show are examples of type classes
-- (+) works with Num

data Employee = Employee {
                  name :: String,
                  position :: String,
                  idNum :: Int
                } deriving (Eq, Show)
                

shawnZhong = Employee {name = "Shawn Zhong", position = "CEO", idNum = 1}
samSimith = Employee {name = "Sam Simth", position = "Manager", idNum = 2}









data ShirtSize =  S | M | L

instance Eq ShirtSize where
  S == S = True
  M == M = True
  L == L = True
  _ == _ = False
  

instance Show ShirtSize where
  show S = "Small"
  show M = "Medium"
  show L = "Large"






class MyEq a where
  areEqual :: a -> a -> Bool
  
instance MyEq ShirtSize where
  areEqual S S = True
  areEqual M M = True
  areEqual L L = True
  areEqual _ _ = False