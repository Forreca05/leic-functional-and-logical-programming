classify :: Int-> String
classify x | x <= 9 = "failed"
            | x > 9 && x < 13 = "passed"
            | x > 12 && x < 16 = "good"
            | x > 15 && x < 19 = "very good"
            | x > 18 && x < 21 = "excellent"

classifyBMI :: Float-> Float-> String
classifyBMI weight height | bmi < 18.5 = "underweight"
                          | bmi >= 18.5 && bmi < 25 = "normal weight"
                          | bmi >= 25 && bmi < 30 = "overweight"
                          | bmi >= 30 = "obese"
                          where bmi = weight / ((^2) height)

max3, min3 :: Ord a => a -> a-> a-> a
max3 x y z = if x>=y && x>=z then x else (if y>=x && y>=z then y else z)
min3 x y z = if x<=y && x<=z then x else (if y<=x && y<=z then y else z)

xor :: Bool-> Bool-> Bool
xor True True = False
xor True False = True
xor False True = True
xor False False = False

safetail :: [a]-> [a]
safetail x | not (null x) = tail x
           | otherwise = []

short :: [a]-> Bool
short x | length x < 3 = True
        | otherwise = False

median :: Ord a => a -> a -> a -> a
median x y z
  | (x >= y && x <= z) || (x >= z && x <= y) = x
  | (y >= x && y <= z) || (y >= z && y <= x) = y
  | otherwise = z

propDivs :: Integer-> [Integer]
propDivs n = [d | d <- [1..n-1], n `mod` d == 0]

perfects :: Integer-> [Integer]
perfects n = [d | d <- [1..n], sum(propDivs d) == d]

pyths :: Integer-> [(Integer,Integer,Integer)]
pyths n = [(x,y,z) | x<-[1..n], y<-[1..n], z<-[1..n], (^2)x + (^2)y == (^2)z]

isPrime :: Integer-> Bool
isPrime n | length [d | d<-[2..n], n `mod` d == 0] < 2 = True
          | otherwise = False

myconcat :: [[a]] -> [a]
myconcat l = [x | xs <-l, x<-xs]

myreplicate :: Int -> a -> [a]
myreplicate n v = [v | _ <- [1..n]]

myIndex :: [a] -> Int -> a
myIndex x d = head[x | (i,x) <- zip [0..] x, d == i]

binom :: Integer-> Integer-> Integer
binom n k = product[1..n] `div` (product[1..k] * product[1..n-k])

pascal :: Integer -> [[Integer]]
pascal n = [[binom x y | y <- [0..x]] | x <- [0..n]]
