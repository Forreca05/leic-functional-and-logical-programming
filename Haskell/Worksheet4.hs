import Data.Char
import Foreign.C (CWchar)
import GHC.IO.Handle (NewlineMode(outputNL))
import Test.QuickCheck



calcPi1 :: Int -> Double
calcPi1 n = 4 * sum (take n terms)
    where 
        terms = [((-1) ** fromIntegral k) / (2 * fromIntegral k + 1) | k <- [0..]]

calcPi2 :: Int -> Double
calcPi2 n = 3 + 4 * sum(take n terms)
    where
        terms = [((-1) ** fromIntegral k) / ((2 * fromIntegral k + 2) + (2 * fromIntegral k + 3) + (2 * fromIntegral k + 4)) | k <- [0..]]

twinPrimes :: [(Integer,Integer)]
twinPrimes = [(a,b) | (a, b) <- zip primes (tail primes), b-a == 2]

primes :: [Integer]
primes = sieve [2..]

sieve :: [Integer] -> [Integer]
sieve (p:xs) = p : sieve [x | x<-xs, x `mod` p/=0]

hamming :: [Integer]
hamming = concat [hammingLayer n | n <- [0..]]

hammingLayer :: Int -> [Integer]
hammingLayer n = [2^i * 3^j * 5^k | i <- [0..n], j <- [0..(n - i)], let k = n - i - j]

rot13Char :: Char -> Char
rot13Char c
    | isUpper c = chr(((ord c - ord 'A' + 13) `mod` 26) + ord 'A')
    | isLower c = chr(((ord c - ord 'a' + 13) `mod` 26) + ord 'a')
    | otherwise = c

rot13 :: String -> String
rot13 = map rot13Char

main :: IO()
main = do
    putStrLn "Enter a text to apply rot13: "
    input <- getLine
    let output = rot13 input
    putStrLn ("Final text:" ++ output)

prop_roundtrip :: String -> Bool
prop_roundtrip xs = rot13(rot13 xs) == xs

type AWord = String
type Line = [AWord]
type Paragraph = [Line]

lineLength :: Line -> Int
lineLength [] = 0
lineLength ws = sum (map length ws) + length ws - 1

fillWords :: Int -> [AWord] -> Paragraph
fillWords _ [] = []
fillWords z (x:xs) = fill [x] xs
    where
        fill current [] = [current]
        fill current (x:xs)
            | lineLength(current ++ [x]) <= z = fill (current ++ [x]) xs
            | otherwise = current : fill [x] xs

formatParagraph :: Paragraph -> String
formatParagraph = unlines . map unwords

main2 :: IO()
main2 = do
    putStrLn "Write words: "
    input <- getContents
    let wordsList = words input
        paragraph = fillWords 70 wordsList
        result = formatParagraph paragraph
    putStrLn "\nFormatted paragraph:\n"
    putStrLn result

type Dict = [String]

readDict :: IO Dict
readDict = do
  txt <- readFile "/usr/share/dict/words"
  return (words txt)

main3 :: IO ()
main3 = do
  dict <- readDict
  putStrLn $ "Número de palavras no dicionário: " ++ show (length dict)
