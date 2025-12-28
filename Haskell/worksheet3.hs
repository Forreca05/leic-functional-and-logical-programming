import System.Win32 (xBUTTON1)
myand :: [Bool]-> Bool
myand [] = True
myand (x:xs) = x && myand xs

myor :: [Bool]-> Bool
myor [] = False
myor (x:xs) = x || myor xs

myconcat :: [[a]]-> [a]
myconcat [[]] = []
myconcat (x:xs) = x ++ myconcat xs

myreplicate :: Int-> a-> [a]
myreplicate 0 _ = []
myreplicate n x = x : myreplicate (n-1) x

myIndex :: [a]-> Int-> a 
myIndex (x:xs) 0 = x
myIndex (x:xs) n = myIndex xs (n-1)

myelem :: Eq a => a-> [a]-> Bool
myelem _ [] = False
myelem d (x:xs) = x == d || myelem d xs

leastDiv :: Integer -> Integer
leastDiv n = head (filter (\d -> n `mod` d == 0) [2..n])

isPrimeFast :: Integer-> Bool
isPrimeFast n = n > 1 && leastDiv n == n 

nub :: Eq a => [a]-> [a]
nub [] = []
nub (x:xs) = x : nub(filter(/=x) xs)

intersperse :: a-> [a]-> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse y (x:xs) = x : y : intersperse y xs

insert :: Ord a => a-> [a]-> [a] 
insert d x = smaller ++ [d] ++ bigger
            where
                smaller = [y | y<-x, y<=d]
                bigger = [z | z<-x, z > d]

isort :: Ord a => [a]-> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

merge :: Ord a => [a]-> [a]-> [a]
merge x [] = x
merge [] y = y
merge (x:xs) (y:ys) | x <= y = x : merge xs (y:ys)
                    | y < x = y : merge (x:xs) ys

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = merge (msort left) (msort right)
  where
    (left, right) = (take (length xs `div` 2) xs, drop (length xs `div` 2) xs)

toBits :: Int-> [Int]
toBits 0 = [0]
toBits 1 = [1]
toBits n = toBits (n `div` 2) ++ [n `mod` 2]

fromBits :: [Int]-> Int
fromBits [] = 0
fromBits (x:xs) = x * (2 ^ length xs) + fromBits xs

divisors :: Integer -> [Integer]
divisors n = filter (\d -> n `mod` d==0) [1..n]

myappend :: [a]-> [a]-> [a]
myappend xs ys = foldr (:) ys xs

mysconcat :: [[a]]-> [a]
mysconcat = foldr (++) []

group :: Eq a => [a]-> [[a]]
group [] = []
group (x:xs) = takeWhile (==x) (x:xs) : group (dropWhile (==x) (x:xs))

