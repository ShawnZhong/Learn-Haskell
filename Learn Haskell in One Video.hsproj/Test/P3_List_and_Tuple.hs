module P3_List_and_Tuple where

import Data.List


-- List
zeroToTen = [0..10]
evenList = [2,4..10]
letterList = ['a'..'z']
infinPow10 = [10,20..]                   -- Haskell is a lazy language

many2s = take 10 (repeat 2)
many3s = replicate 10 3 
cycleList = take 10 (cycle [1,2,3,4,5])

listTimes2 = [x * 2| x <- [1..10]]
listTimes3 = [x * 3| x <- [1..10], x * 3 <= 50]
divisiBy9N13 = [x | x <- [1..500], x `mod` 13 == 0, x `mod` 9 == 0]

sortedList = sort [6,4,2,2,8,0,1]        -- need to import Data.List

sumOfLists = zipWith (+) [1,2,3,4] [0,7,3,1]

filteredList = filter (>5) evenList

evensUpTo20 = takeWhile (<= 20) [2,4..]

multOfList = foldl (*) 1 [2,3,4,5]
multOfList' = foldr (*) 1 [2,3,4,5]

pow3List = [3 ^ x | x <- [1..10] ]


multTable = [[x * y | y <- [1..10]] | x <- [1..10]]






-- Tuple
randTuple = (1, "Random Tuple")

bobSmith = ("Bob Somith", 52)

bobsName = fst bobSmith
bobsAge = snd bobSmith

names = ["Bob", "Mary", "Tom"]
address = ["123 Main", "234 North", "567 South"]
namesNAddress = zip names address