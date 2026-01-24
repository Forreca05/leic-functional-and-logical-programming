algarismosRev :: Int -> [Int]
algarismosRev 0 = []
algarismosRev n = n `mod` 10 : algarismosRev (n `div` 10)

algarismos :: Int -> [Int]
algarismos n = reverse (algarismosRev n)

toBitsAux :: Int -> [Int]
toBitsAux 1 = [1]
toBitsAux n = n `mod` 2 : toBitsAux (n `div` 2)

toBits :: Int -> [Int]
toBits n = reverse (toBitsAux n)

fromBits :: [Int] -> Int
fromBits [] = 0
fromBits (x:xs)
     | x == 1 = 2^length xs + fromBits xs
     | otherwise = fromBits xs

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x xs = smaller ++ [x] ++ bigger
    where 
        smaller = [y| y <-xs, y <= x]
        bigger = [y| y <-xs, y > x]

sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = insert x (sort xs)

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) 
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

