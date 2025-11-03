{-
  A basic calculator for arithmetic expressions
  Based on the example in Chapter 8 of "Programming in Haskell"
  by Graham Hutton.

  Pedro Vasconcelos, 2025
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Main where

import Parsing
import Data.Char
import GHC.IO.Encoding (TextEncoding(textEncodingName))

--
-- a data type for expressions
-- made up from integer numbers, + and *
--
type Name = String
type Env = [(Name, Integer)]

data Expr = Num Integer
          | Var String
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Rem Expr Expr
          deriving Show

data Command = Assign String Expr
             | EvalExpr Expr
             deriving Show

-- a recursive evaluator for expressions

eval :: Env -> Expr -> Integer
eval env (Num n)     = n
eval env (Var x)     = case lookup x env of
                         Just v  -> v
                         Nothing -> error ("undefined variable: " ++ x)
eval env (Add a b)   = eval env a + eval env b
eval env (Sub a b)   = eval env a - eval env b
eval env (Mul a b)   = eval env a * eval env b
eval env (Div a b)   = eval env a `div` eval env b
eval env (Rem a b)   = eval env a `rem` eval env b



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
      <|> do char '('
             e <- expr
             char ')'
             return e
      <|> do v <- variable
             return (Var v)       
             

natural :: Parser Integer
natural = do xs <- many1 (satisfy isDigit)
             return (read xs)

variable :: Parser String
variable = do xs <- many1 (satisfy isAlpha)
              return xs

command :: Parser Command
command = do v <- variable
             char '='
             e <- expr
             return (Assign v e)
      <|> do e <- expr
             return (EvalExpr e)

----------------------------------------------------------------             
  
main :: IO ()
main
  = do txt <- getContents
       calculator [] (lines txt)

-- | read-eval-print loop
calculator :: Env -> [String] -> IO ()
calculator _ []  = return ()
calculator env (l:ls) = do
  let (out, env') = execute env l
  putStrLn out
  calculator env' ls


execute :: Env -> String -> (String, Env)
execute env txt
  = case parse command txt of
    [(Assign v e, "")] -> let val = eval env e
                              env' = (v, val) : env
                          in (show val, env')
    [(EvalExpr e, "")] -> let val = eval env e
                          in (show val, env)
    _ -> ("parse error; try again", env)