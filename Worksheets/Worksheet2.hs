
classify :: Int -> String
classify x | x <= 9 = "failed"
            | x <= 12 = "passed"
            | x <= 15 = "good"
            | x <= 18 = "very good"
            | otherwise = "excellent"

classifyBMI :: Float -> Float -> String
classifyBMI weight height
    | bmi <= 18.5 = "underweight"
    | bmi <= 24.9 = "normal weight"
    | bmi <= 29.9 = "overweight"
    | otherwise = "obesity"
    where bmi = weight / (height * height)

max3, min3 :: Ord a => a -> a -> a -> a
max3 x y z = max x (max y z)
min3 x y z = min x (min y z)

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

-- A version using conditional expressions
safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

-- A version using guarded equations
--safetail :: [a] -> [a]
--safetail xs
--  | null xs   = []
--  | otherwise = tail xs

-- A version using pattern matching
--safetail :: [a] -> [a]
--safetail [] = []
--safetail (_:xs) = xs

short :: [a] -> Bool
short xs = length xs < 3

--short :: [a] -> Bool
--short [] = True
--short [_] = True
--short [_, _] = True
--short (_:_:_:_) = False

--median :: Ord a => a -> a -> a -> a
--median x y z
--  | (x <= y && y <= z) || (z <= y && y <= x) = y
--  | (y <= x && x <= z) || (z <= x && x <= y) = x
--  | otherwise                                = z

median :: (Ord a, Num a) => a -> a -> a -> a
median x y z = x + y + z - min3 x y z - max3 x y z



propDivs :: Integer -> [Integer]
propDivs n = [x | x <- [1..n-1], n `mod` x == 0]

perfects :: Integer -> [Integer]
perfects n = [x | x <- [1..n-1], sum (propDivs x) == x]

pyths :: Integer -> [(Integer,Integer,Integer)]
pyths n = [(x,y,z) | x <- [1..n], y <- [x..n], z <- [y..n], x^2 + y^2 == z^2]

isPrime :: Integer -> Bool
isPrime n = length [x | x <- [1..n], n `mod` x == 0] == 2

myconcat :: [[a]] -> [a]
myconcat xss = [x | xs <- xss, x <- xs]

myreplicate :: Int -> a -> [a]
myreplicate n x = [x | _ <- [1..n]]

myindex :: [a] -> Int -> a
myindex xs n = head [x | (i,x) <- zip [0..] xs, i == n]

binom :: Integer -> Integer -> Integer
binom n k = product [1..n] `div` (product [1..k] * product [1..(n - k)])

pascal :: Integer -> [[Integer]]
pascal n = [[binom i k | k <- [0..i]] | i <- [0..n]]