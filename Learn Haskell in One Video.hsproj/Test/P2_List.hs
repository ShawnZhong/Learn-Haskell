module P2_List where

primes = [3, 5, 7, 11]                   -- use [] to create a list

morePrimes = primes ++ [13, 17, 19, 23]  -- use ++ to concate two lists

evenMorePrimes = 2 : morePrimes          -- use : to add item at the beginning of a list
favNums = 2 : 7 : 21 : 66 : []


nestedList = [[3, 5, 7], [11, 13, 17]] 

 
lenPrime = length evenMorePrimes

revPrime = reverse evenMorePrimes

isListEmpty = null evenMorePrimes
isListEmpty' = null []

secondPrime = evenMorePrimes !! 1        -- indexed at 0
firstPrime = head evenMorePrimes
lastPrime = last evenMorePrimes

primeInit = init evenMorePrimes          -- drop the last item

first3Prime = take 3 evenMorePrimes
removedPrime = drop 3 evenMorePrimes     -- drop the first n items

is7inList = 7 `elem` evenMorePrimes

maxPrime = maximum evenMorePrimes
minPrime = minimum evenMorePrimes

sumPrime = sum evenMorePrimes
prodPrime = product evenMorePrimes