module Ch6_1 where

-- Create a new data type called `LineStyle`
-- Solid, Dashed, Dotted are called the data constructors
-- Data constructors must start with an uppercase letter
data LineStyle = Solid | Dashed | Dotted

type Point = (Float, Float)
type Line  = (Point, Point)
type FancyLine = (Point, Point, LineStyle)

myLine :: FancyLine
myLine = ((0, 0), (1, 1), Dashed)

changeLineStyle :: FancyLine -> LineStyle -> FancyLine
changeLineStyle (x, y, _) newStyle = (x, y, newStyle)








-- Enumeration Types
data Day =
  Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
  deriving (Enum, Show, Eq, Ord)
-- include the new type Day into the standard type class Enum
-- [Monday .. Friday] == [Monday, Tuesday, Wednesday, Thursday, Friday]







-- Pattern matching and case expressions
isWeekday :: Day -> Bool
isWeekday Sunday   = False
isWeekday Saturday = False
isWeekday _        = True --If no pattern matches, evaluation raises a exception

-- Equivalent Syntaxs
isWeekday' :: Day -> Bool
isWeekday' day = case day of
                  Sunday   -> False
                  Saturday -> False
                  _        -> True
                  

isWeekday'' :: Day -> Bool
isWeekday'' day = case day of {Sunday -> False ; Saturday -> False; _ -> True}


isWeekend :: Day -> Bool
isWeekend day = day `elem` [Saturday, Sunday]











