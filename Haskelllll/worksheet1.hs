incr :: Int-> Int
incr x = x+1

triple :: Int-> Int
triple x = 3*x

welcome :: String-> String
welcome name = "Hello, " ++ name ++ "!"
 
count :: String-> String
count str = show (length str) ++ " characters."

leftHalf :: [Int] -> [Int]
leftHalf x = take (length x `div` 2) x

rightHalf :: [Int] -> [Int]
rightHalf x = drop (length x `div` 2) x

second :: [Int] -> Int
second x = head (drop 1 x)

lasts :: [Int] -> Int
lasts x = head (drop (length x - 1) x)

inits :: [Int] -> [Int]
inits x = reverse (drop 1 (reverse x))

middle :: [Int] -> Int
middle x = head (drop (length x `div` 2) x)

checkPalindrome :: String -> Bool
checkPalindrome s | s == reverse s = True
                  | otherwise = False

checkTriangle :: Float-> Float-> Float-> Bool
checkTriangle a b c = if (a+b > c) && (b+c > a) && (c+a > b) then True else False

triangleArea :: Float-> Float-> Float-> Float
triangleArea a b c = sqrt (s * (s-a) * (s-b) * (s-c))
    where s = (a+b+c) / 2