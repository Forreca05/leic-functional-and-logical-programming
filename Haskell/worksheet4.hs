import Data.Char

calcPi1 :: Int-> Double
calcPi1 n = 4 * sum (take n terms)
        where
            terms = [fromIntegral ((-1) ^ k) / fromIntegral (2*k + 1) | k <- [0..]]

calcPi2 :: Int-> Double
calcPi2 n = 3 + 4 * sum (take n terms)
        where
            terms = [fromIntegral((-1) ^ k+1) / fromIntegral(2*k * (2*k+1) * (2*k+2)) | k <- [1..]]

twinPrimes :: [(Integer,Integer)]
twinPrimes = [(a,b) | (a,b) <- zip primes (tail primes), b-a==2]

primes :: [Integer]
primes = sieve [2..]

sieve :: [Integer] -> [Integer]
sieve (p:xs) = p : sieve [x | x<-xs, x `mod` p/=0]

hamming :: [Integer]
hamming = concat [hamminglayeer n | n <- [0..]]

hamminglayeer :: Integer -> [Integer]
hamminglayeer n = [2^i * 3^j * 5^k | i <- [0..n], j<-[0..(n-i)], let k = n-i-j]

merge :: [Integer]-> [Integer]-> [Integer]
merge (x:xs) (y:ys) | x < y = x : merge xs (y:ys)
                    | x == y = x : merge xs ys
                    | otherwise = y : merge (x:xs) ys

rot13char :: Char -> Char
rot13char c | isUpper c = chr((ord c - ord 'A' + 13) `mod` 26 + ord 'A')
            | isLower c = chr((ord c - ord 'a' + 13) `mod` 26 + ord 'a')
            | otherwise = c

rot13 :: String -> String
rot13 = map rot13char

main :: IO()
main = do
    putStrLn "Enter a text to apply rot13: "
    input <- getLine
    let output = rot13 input
    putStrLn ("Final text:" ++ output)

type AWord = String
type Line = [AWord]
type Paragraph = [Line]

lineLength :: [AWord] -> Int
lineLength [] = -1
lineLength (x:xs) = length x + 1 + lineLength xs

fillWords :: Int-> [AWord]-> Paragraph
fillWords _ [] = []
fillWords n (x:xs) = fill [x] xs
            where fill current [] = [current]
                  fill current (x:xs) | lineLength current + length x + 1 <= n = fill (current ++ [x]) xs
                                      | otherwise = current : fill [x] xs
                    
type Dict = [String]

readDict :: IO Dict
readDict = do
  txt <- readFile "/usr/share/dict/words"
  return (words txt)

slength :: IO Int
slength = do
  dict <- readDict
  return (length dict)
