import Data.Char (isUpper, isLower, isDigit)

divprop :: Integer -> [Integer]
divprop x = [s|s <- [1..x-1], x `mod` s == 0]

pascal :: Integer -> [[Integer]]
pascal n = [[binom x y | y <- [0..x]] | x <- [0..n]]

binom :: Integer -> Integer -> Integer
binom n k = factorial n (n-k+1) `div` (factorial k 1)
     where
         factorial n i = product [i..n]

myand :: [Bool] -> Bool
myand [] = True
myand (x:xs) = x && myand xs

myor :: [Bool] -> Bool
myor [] = False
myor (x:xs) = x || myor xs

forte :: String -> Bool
forte x = length x > 7 && any isUpper x && any isLower x && any isDigit x

intersperse _ [] = []
intersperse _ [x] = [x]
intersperse y (x:xs) = x : y : intersperse y xs

