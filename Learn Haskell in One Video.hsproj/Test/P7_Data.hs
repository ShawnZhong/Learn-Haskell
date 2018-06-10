module P7_Data where
  
data BaseballPlayer = Pitcher | Catcher | Infielder | Outfield
  deriving Show
  
barryBonds :: BaseballPlayer -> Bool
barryBonds Outfield = True




data Customer = Customer String String Double deriving Show

tomSmith :: Customer 
tomSmith = Customer "Tom Smith" "123 Main" 20.50

getBalance :: Customer -> Double
getBalance (Customer _ _ b) = b 




data RPS = Rock | Paper | Scissors
shoot :: RPS -> RPS -> String
shoot Paper     Rock      = "Paper Beats Rock"
shoot Rock      Scissors  = "Rock Beats Scissors"
shoot Scissors  Paper     = "Scissors Beats Paper"
shoot Scissors  Rock      = "Scissors Loses to Rock"
shoot Paper     Scissors  = "Paper Loses to Scissors"
shoot Rock      Paper     = "Rock Loses to Paper"
shoot _         _         = "Error"



data Shape = Circle Float Float Float 
           | Rectangle Float Float Float Float
           deriving Show
           
area :: Shape -> Float
area (Circle _ _ r) = pi * r ^ 2
area (Rectangle x1 y1 x2 y2) = abs $ (x1 - x2) * (y1 - y2)
-- Note: We can use $ to remove () after it



sumVal = putStrLn(show(1 + 2))
sumVal' = putStrLn . show $ 1 + 2