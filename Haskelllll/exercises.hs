import Data.Char
import Distribution.Compat.Lens (_1)
import System.Win32 (COORD(yPos), fILE_READ_ATTRIBUTES)
import Control.Arrow (ArrowChoice(right))

testaTriangulo :: Float -> Float -> Float -> Bool
testaTriangulo x y z = (x+y>z) && (y+z>x) && (z+x>y)

areaTriangulo :: Float -> Float -> Float -> Float
areaTriangulo x y z = sqrt(s*(s-x)*(s-y)*(s-z))
                    where
                        s = (x+y+z)/2

binom :: Integer -> Integer -> Integer
binom n k
  | k < 0 || k > n = 0
  | otherwise      = product [n-k+1 .. n] `div` factorial k
  where
    factorial g = product [1..g]

classifica :: Float -> Float -> String
classifica x y | imc < 18.5 = "baixo peso"
               | imc >= 18.5 && imc < 25 = "peso normal"
               | imc >= 25 && imc < 30 = "excesso de peso"
               | imc >= 30 = "obesidade"
            where
              imc = x / (^2) y

max3 :: Int -> Int -> Int -> Int
max3 x y z | x >= y && x >= z = x
           | y >= x && y >= z = y
           | otherwise = z

xor :: Bool -> Bool -> Bool
xor x y | x == y = False
        | otherwise = True

min3 :: Int -> Int -> Int -> Int
min3 x y z | x <= y && x <= z = x
           | y <= x && y <= z = y
           | otherwise = z

mediana :: Int -> Int -> Int -> Int
mediana x y z = x + y + z - max3 x y z - min3 x y z

divprop :: Integer -> [Integer]
divprop n = [d | d<-[1..n-1], n `mod` d == 0]

factorial :: Integer -> Integer -> Integer
factorial n k = fact n `div` (fact k * fact(n-k))
          where
            fact n = product [1..n]

pascal :: Integer -> [[Integer]]
pascal n = [pascalAux n | n<-[0..n]]

pascalAux :: Integer -> [Integer]
pascalAux n = [factorial n k | k <- [0..n]]

myand :: [Bool] -> Bool
myand = foldr (&&) True

myor :: [Bool] -> Bool
myor [] = False
myor (x:xs) = x || myor xs

forte :: String -> Bool
forte xs = length xs > 7 &&  (any isUpper xs) && (any isLower xs) && (any isDigit xs)

intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [n] = [n]
intersperse c (x:xs) = x : c : intersperse c xs

algarismos :: Int -> [Int]
algarismos = reverse . algarismosRev 

algarismosRev :: Int -> [Int]
algarismosRev 0 = []
algarismosRev n = n `mod` 10 : algarismosRev (n `div` 10)

toBits :: Int -> [Int]
toBits n = reverse (toBitsAux n)

toBitsAux :: Int -> [Int]
toBitsAux 0 = []
toBitsAux n = (n `mod` 2) : toBitsAux (n `div` 2)

fromBits :: [Int] -> Int
fromBits [] = 0
fromBits (x:xs) = x * 2 ^ length xs + fromBits xs

insert :: Ord a => a -> [a] -> [a]
insert y [] = [y]
insert y (x:xs) | y < x = y : x : xs
                | otherwise = x : insert y xs

sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = smaller x xs ++ [x] ++ bigger x xs
        where 
          smaller x xs = [d | d <- sort xs, d <= x]
          bigger x xs = [d | d <- sort xs, d > x]

merge :: Ord a => [a] -> [a] -> [a]
merge [] y = y
merge x [] = x
merge (x:xs) (y:ys) | x <= y = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys



{-main :: IO ()
main = do 
        input <- getContents
        let linhas = lines input
        print (length linhas)
        let palavras = map words linhas
        print (sum(map length palavras))
        let caracteres = map length linhas
        print (sum(caracteres) + length linhas)-}

{-main :: IO()
main = do 
        input <- getContents
        let linhas = lines input
        let linhasr = map reverse linhas
        putStr (unlines linhasr)-}

rot13 :: Char -> Char
rot13 c | isUpper c = chr(((ord c - ord 'A' + 13) `mod` 26) + ord 'A')
        | isLower c = chr(((ord c - ord 'a' + 13) `mod` 26) + ord 'a')
        | otherwise = c

main :: IO()
main = do 
        input <- getContents
        let final = map rot13 input
        putStrLn final

primo :: Integer -> Bool
primo n | n <= 1 = False
        | otherwise = all(\d -> n `mod` d /= 0) [2 .. limite]
      where
        limite = floor (sqrt (fromIntegral n))

{-zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f x [] = []
zipWith f [] y = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys-}

palavras :: String -> [String]
palavras [] = []
palavras xs = 
  let w = takeWhile (not . isSpace) xs 
      ws = dropWhile (not . isSpace) xs
      wx = dropWhile isSpace xs
  in if null w then palavras wx
     else w : palavras ws

aproxPi1 :: Int -> Double
aproxPi1 n = 4 * sum(take n terms)
        where 
            terms = [((-1)^k) / fromIntegral (2 * k + 1) | k <-[0..]]

aproxPi2 :: Int -> Double
aproxPi2 n = 3 + 4 * sum(take (n-1) terms)
        where
            terms = [((-1)^(k+1))/ fromIntegral(2*k * (2*k+1) * (2*k+2)) | k <- [1..]]

{-pascal :: [[Integer]]
pascal = [linha n | n<-[0..]]

binomial :: Integer -> Integer -> Integer
binomial n k = factor n `div` (factor k * factor (n-k))
        where
            factor m = product [1..m]

linha :: Integer -> [Integer]
linha n = [binomial n k | k<-[0..n]]-}

goldbach :: Integer -> (Integer, Integer)
goldbach n 
  | n > 2 && even n = head [(a,b) | a <- takeWhile (<n) primes,
                                    b <- takeWhile (<n) primes,
                                    a + b == n]
  | otherwise       = (0,0)

primes :: [Integer]
primes = sieve [2..]

sieve :: [Integer] -> [Integer]
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

-- tipo para expressÃµes proposicionais
data Prop = Const Bool        -- constantes
          | Var Char          -- variÃ¡veis
          | Neg Prop          -- negaÃ§Ã£o
          | Conj Prop Prop    -- conjunÃ§Ã£o
          | Disj Prop Prop    -- disjunÃ§Ã£o
          | Impl Prop Prop    -- implicaÃ§Ã£o
            deriving (Eq,Show)

satisfaz :: Prop -> Bool
satisfaz (Const x) = x
satisfaz (Var x) = False
satisfaz (Neg x) = not (satisfaz x)
satisfaz (Conj x y) = satisfaz x && satisfaz y
satisfaz (Disj x y) = satisfaz x || satisfaz y
satisfaz (Impl x y) = not (satisfaz x) || satisfaz y

data Arv a = Vazia | No a (Arv a) (Arv a)

sumArv :: Num a => Arv a -> a
sumArv Vazia = 0
sumArv (No n (left) (right)) = n + sumArv left + sumArv right

nivel :: Int -> Arv a -> [a]
nivel 0 Vazia = []
nivel 0 (No n (left) (right)) = [n]
nivel n Vazia = []
nivel n (No f (left) (right)) = nivel (n-1) left ++ nivel (n-1) right


maxpos :: [Int] -> Int
maxpos [] = 0
maxpos (x:xs) | x > maxpos xs = x
              | otherwise = maxpos xs

dups :: [a] -> [a]
dups [] = []
dups [x] = x : [x]
dups (x:xs:xy) = x : x : xs : dups xy

transforma :: String -> String
transforma [] = []
transforma (x:xs) | x `elem` ['a','o','e','i','u'] = x : 'p' : x : transforma xs
                  | otherwise = x : transforma xs

type Species = (String, Int)
type Zoo = [Species]

isEndangered :: Species -> Bool
isEndangered (x,y) = if y <= 100 then True else False

updateSpecies :: Species -> Int -> Species
updateSpecies (x,y) count = (x,y+count)

filterSpecies :: Zoo -> (Species -> Bool) -> Zoo
filterSpecies [] _ = []
filterSpecies (x:s) f | f x == True = x : filterSpecies s f
                      | otherwise = filterSpecies s f