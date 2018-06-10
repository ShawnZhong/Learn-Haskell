module P3_Tuple where
  
randTuple = (1, "Random Tuple")

bobSmith = ("Bob Somith", 52)

bobsName = fst bobSmith
bobsAge = snd bobSmith

names = ["Bob", "Mary", "Tom"]
address = ["123 Main", "234 North", "567 South"]
namesNAddress = zip names address


getTriple x = x * 3