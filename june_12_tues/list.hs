prod :: [Int] -> Int
prod []     = 1
prod (x:xs) = x * prod xs

allTrue :: [Bool] -> Bool
allTrue []     = True
allTrue (x:xs) = x && allTrue xs

allFalse :: [Bool] -> Bool
allFalse []     = True
allFalse (x:xs) = (not x) && allFalse xs

decAll :: [Int] -> [Int]
decAll []     = []
decAll (x:xs) = x-1 : decAll xs

convertIntBool :: [Int] -> [Bool]
convertIntBool []      = []
convertIntBool (0:xs)  = False : convertIntBool xs
convertIntBool (x:xs)  = True  : convertIntBool xs

pairUp :: [a] -> [b] -> [(a,b)]
pairUp  []     []     = []
pairUp  (x:xs) []     = []
pairUp  []     (y:ys) = []
pairUp  (x:xs) (y:ys) = (x,y) : pairUp xs ys

takePrefix :: Int -> [a] -> [a]
takePrefix num []     = []
takePrefix 0   xs     = []
takePrefix num (x:xs) = x : takePrefix (num-1) xs

dropPrefix :: Int -> [a] -> [a]
dropPrefix 0   xs     = xs
dropPrefix num []     = []
dropPrefix num (x:xs) = dropPrefix (num-1) xs


