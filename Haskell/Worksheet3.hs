myAnd :: [Bool] -> Bool
myAnd [] = True                 
myAnd (x:xs) = x && myAnd xs  

myOr :: [Bool] -> Bool
myOr [] = True
myOr (x:xs) = x || myOr xs

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (x:xs) = x ++ myConcat xs

myReplicate :: Int -> a -> [a]
myReplicate 0 _ = []                     
myReplicate n x = x : myReplicate (n-1) x 

myIndex :: [a] -> Int -> a
myIndex (x:_) 0 = x                    
myIndex (_:xs) n = myIndex xs (n-1)     
myIndex [] _ = error "Index out of bounds"  

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem y (x:xs)
  | y == x    = True
  | otherwise = myElem y xs

leastDiv :: Integer -> Integer
leastDiv = leastDivFrom 2 

leastDivFrom :: Integer -> Integer -> Integer
leastDivFrom d n 
    | d * d > n = n
    | n `mod` d == 0 = d
    | otherwise = leastDivFrom (d+1) n

isPrimeFast :: Integer -> Bool
isPrimeFast n = n > 1 && leastDiv n == n

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (filter (/= x) xs)

intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse k (x:xs) = x : k : intersperse k xs

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
    | x <= y = y : x : ys
    | otherwise = y : insert x ys

isort :: Ord a => [a] -> [a]
isort [] = []                    
isort (x:xs) = insert x (isort xs)  

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x <= y    = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = merge (msort left) (msort right)
  where
    (left, right) = splitAt (length xs `div` 2) xs

toBits :: Int -> [Int]
toBits 0 = [0]
toBits 1 = [1]
toBits n = toBits (n `div` 2) ++ [n `mod` 2]

fromBits :: [Int] -> Int
fromBits [] = 0
fromBits (b:bs) = b * 2 ^ length bs + fromBits bs

divisors :: Integer -> [Integer]
divisors n = filter (\d -> n `mod` d == 0) [1..n]

--isPrimeFast :: Integer -> Bool
--isPrimeFast n
--  | n < 2     = False
--  | otherwise = all (\d -> n `mod` d /= 0) [2..n-1]

myappend :: [a] -> [a] -> [a]
myappend xs ys = foldr (:) ys xs

myconcat :: [[a]] -> [a]
myconcat = foldr (++) []

myreverseR :: [a] -> [a]
myreverseR = foldr (\x acc -> acc ++ [x]) []

myreverseL :: [a] -> [a]
myreverseL = foldl (\acc x -> x : acc) []

myelem :: Eq a => a -> [a] -> Bool
myelem x = any(==x)

--fromBits :: [Int] -> Int
--fromBits = foldl (\acc b -> acc * 2 + b) 0

group :: Eq a => [a] -> [[a]]
group [] = []
group (x:xs) = takeWhile (== x) (x:xs) : group (dropWhile (==x) (x:xs))

intercalate :: a -> [a] -> [[a]]
intercalate x [] = [[x]]
intercalate x (y:ys) = (x:y:ys) : map (y:) (intercalate x ys)

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x:xs) = combine (permutations xs)
  where
    combine [] = []
    combine (p:ps) = intercalate x p ++ combine ps


  
