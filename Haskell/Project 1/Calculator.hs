{-
  A basic calculator for arithmetic expressions
  Based on the example in Chapter 8 of "Programming in Haskell"
  by Graham Hutton.

  Pedro Vasconcelos, 2025
-}
module Main where

import Parsing
import Data.Char

--
-- a data type for expressions
-- made up from integer numbers, + and *
--
data Expr = Num Integer
          | Var String
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Rem Expr Expr
          deriving Show

data Command = Assign String Expr
             | Eval Expr
             deriving Show

type Name = String
type Env = [(Name, Integer)]
-- a recursive evaluator for expressions
--
eval :: Env -> Expr -> Integer
eval _ (Num n) = n
eval env (Var x) = case lookup x env of
                     Just b -> b
                     Nothing -> error "undefinid variable"
eval env (Add e1 e2) = eval env e1 + eval env e2
eval env (Sub e1 e2) = eval env e1 - eval env e2
eval env (Mul e1 e2) = eval env e1 * eval env e2
eval env (Div e1 e2) = eval env e1 `div` eval env e2
eval env (Rem e1 e2) = eval env e1 `mod` eval env e2

-- | a parser for expressions
-- Grammar rules:
--
-- expr ::= term exprCont
-- exprCont ::= '+' term exprCont | epsilon

-- term ::= factor termCont
-- termCont ::= '*' factor termCont | epsilon

-- factor ::= natural | '(' expr ')'

expr :: Parser Expr
expr = do t <- term
          exprCont t

exprCont :: Expr -> Parser Expr
exprCont acc = do char '+'
                  t <- term
                  exprCont (Add acc t)
              <|> do char '-'
                     t <- term
                     exprCont (Sub acc t)
              <|> return acc
              
term :: Parser Expr
term = do f <- factor
          termCont f

termCont :: Expr -> Parser Expr
termCont acc =  do char '*'
                   f <- factor  
                   termCont (Mul acc f)
                <|> do char '/'
                       f <- factor
                       termCont (Div acc f)
                <|> do char '%'
                       f <- factor
                       termCont (Rem acc f)
                <|> return acc

factor :: Parser Expr
factor = do n <- natural
            return (Num n)
          <|>
          do char '('
             e <- expr
             char ')'
             return e
          <|> do v <- variable
                 return (Var v)
             
command :: Parser Command
command = do v <- variable
             char '='
             e <- expr
             return (Assign v e)
          <|> do e <- expr
                 return (Eval e)
                 
natural :: Parser Integer
natural = do xs <- many1 (satisfy isDigit)
             return (read xs)

variable :: Parser String
variable = do xs <- many1 (satisfy isLetter)
              return xs

----------------------------------------------------------------             
  
main :: IO ()
main
  = do txt <- getContents
       calculator [] (lines txt)

-- | read-eval-print loop
calculator :: Env -> [String] -> IO ()
calculator _ []  = return ()
calculator env (l:ls) = do 
                    let (v, out) = execute env l
                    putStrLn v
                    calculator out ls

execute :: Env -> String -> (String, Env)
execute env txt
  = case parse command txt of 
      [(Assign v e, "")] -> let val = eval env e
                                env' = (v, val) : env
                            in (show val, env')
      [(Eval e, "")] -> let val = eval env e
                        in (show val, env)
      _ -> ("parse error; try again", env)  