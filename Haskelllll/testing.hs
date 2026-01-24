import Data.Char
import Distribution.Compat.Lens (_1)
import Data.Text.Lazy (splitOn, count)
import Control.Monad.Cont (cont)
import Data.List (intercalate)
--Matriculas de automóveis, alínea A

type Letras = (Char,Char)          -- um bloco de letras
type Digitos = (Int,Int)           -- um bloco de algarismos
type Matricula = (Letras, Digitos, Letras)  -- uma matrícula

valida :: Matricula -> Bool
valida (d,f,g) = isletra d && isdigitio f && isletra g

isletra :: Letras -> Bool
isletra (x,xs) = (x>='A' && x<='Z') && (xs>='A' && xs<='Z')

isdigitio :: Digitos -> Bool
isdigitio (x,xs) = (x>=0 && x<=9) && (xs>=0 && xs<=9)


--Matriculas de automóveis, alínea B

incrMatricula :: Matricula -> Matricula 
incrMatricula (d,f,g) | g /= ('Z','Z') = (d,f,incrletras g)
                      | g == ('Z','Z') && f /= (9,9) = (d,incrdigitios f, incrletras g)
                      | otherwise = (incrletras d, incrdigitios f, incrletras g)



incrdigitios :: Digitos -> Digitos
incrdigitios (x,y) | y < 9 = (x,y+1)
                   | y == 9 && x < 9 = (x+1,0)
                   | otherwise = (0,0)

incrletras :: Letras -> Letras
incrletras (x,y) | y < 'Z' = (x, chr(ord y + 1))
                 | y == 'Z' && x < 'Z' = (chr(ord x + 1), 'A')
                 | otherwise = ('A', 'A')


--Metodo de Hondt alinea A

maxIndex :: Ord a => [a] -> (a, Int)
maxIndex xs = head (reverse [(x,y) | (x,y) <- zip xs [0..], x == maximum xs])


--Formatar Texto alinea A
paragraphs :: String -> [String]
paragraphs "" = []
paragraphs s =
  let (p, rest) = breakOnDoubleNewline s
  in p : case rest of
           []       -> []
           (_:_:xs) -> paragraphs xs  -- descarta os dois '\n'
           _        -> []             -- segurança extra

-- função auxiliar que parte na primeira ocorrência de "\n\n"
breakOnDoubleNewline :: String -> (String, String)
breakOnDoubleNewline [] = ([], [])
breakOnDoubleNewline ('\n':'\n':xs) = ([], '\n':'\n':xs)
breakOnDoubleNewline (c:cs) =
  let (p, rest) = breakOnDoubleNewline cs
  in (c:p, rest)

--Formatar Texto alinea B
fillWords :: Int -> [String] -> [[String]]
fillWords _ [] = []
fillWords n xs = take (wordsThatFit n xs) xs : fillWords n (drop (wordsThatFit n xs) xs)

wordsThatFit :: Int -> [String] -> Int
wordsThatFit _ [] = 0
wordsThatFit n (x:xs) = fill n (x:xs) 0
                where 
                    fill n [] cont = cont
                    fill n (x:xs) cont | length(x) < n = fill (n-length(x) - 1) xs (cont+1)
                                       | length(x) == n = cont + 1
                                       | otherwise = cont

--Formatar Texto alinea C
main :: IO()
main = do
        input <- getContents
        let paras = paragraphs input
            formatted = map (unlines . map unwords . fillWords 70 . words) paras
        putStr (intercalate "\n\n" formatted)

