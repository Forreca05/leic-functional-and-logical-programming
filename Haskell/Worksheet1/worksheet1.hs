incr :: Int -> Int
incr x = x + 1

triple :: Int -> Int
triple x = 3 * x

welcome :: String -> String
welcome name = "Hello, " ++ name ++ "!"

count :: String -> String
count str = show (length str) ++ " characters."

leftHalf :: [Int] -> [Int]
leftHalf x = take (div (length x) 2) x 

rightHalf :: [Int] -> [Int]
rightHalf x = drop (div(length x ) 2) x

second :: [Int] -> Int
second x = head (tail x) 

last :: [Int] -> Int
last x = head (reverse x)

init :: [Int] -> [Int]
init x = reverse (tail (reverse x))

middle :: [Int] -> Int
middle x = x !! div (length x) 2

checkPalindrome :: String -> Bool
checkPalindrome x = x == reverse x

checkTriangle :: Float -> Float -> Float -> Bool
checkTriangle a b c = ((a + b) > c) && ((b + c) > a) && ((c + a) > b)

triangleArea :: Float-> Float-> Float-> Float
triangleArea a b c = sqrt (s * (s-a) * (s-b) * (s-c))
    where s = (a+b+c) / 2
