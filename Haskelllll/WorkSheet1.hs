leftHalf :: [Int] -> [Int]
leftHalf x = take (div (length x) 2) x 

rightHalf :: [Int] -> [Int]
rightHalf x = drop (div (length x) 2) x 

second :: [Int] -> Int
second x = head (tail x)

last :: [Int] -> Int
last x = head (reverse x)

init :: [Int] -> [Int]
init x = take (length x - 1) x

middle :: [Int] -> Int
middle x = head (rightHalf x)

checkPalindrome :: String -> Bool
checkPalindrome x = x == reverse x

checkTriangle :: Float -> Float -> Float -> Bool
checkTriangle a b c = ((a + b) > c) && ((a + c) > b) && ((b + c) > a)

triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c = sqrt(s * (s - a) * (s - b) * (s - c))
    where s = (a + b + c) / 2
