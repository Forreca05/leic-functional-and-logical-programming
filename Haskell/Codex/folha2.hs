classifica :: Float -> Float -> String
classifica weight height 
   | imc < 18.5 = "baixo peso"
   | imc < 25 = "peso normal"
   | imc < 30 = "excesso de peso"
   | otherwise = "obesidade"
   where 
      imc = weight / height ** 2   

max3 :: Int -> Int -> Int -> Int
max3 a b c = max a (max b c)

min3 :: Int -> Int -> Int -> Int
min3 a b c = min a (min b c)

xor :: Bool -> Bool -> Bool
xor a b 
   | a == b = False 
   | otherwise = True

mediana :: Int -> Int -> Int -> Int
mediana a b c = (max3 a b c + min3 a b c - a - b - c) * (-1)

