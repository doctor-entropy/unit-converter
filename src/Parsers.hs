{-# LANGUAGE OverloadedStrings #-}

module Parsers where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

freespace :: Parser a -> Parser a
freespace p = do space
                 v <- p
                 space
                 return v

symbol :: String -> Parser String
symbol xs = freespace $ string xs

wholeNum :: Parser Int
wholeNum = L.decimal

int :: Parser Int
int = L.signed space wholeNum

integer :: Parser Int
integer = freespace int

longflag :: Parser String
longflag = do
    string "--"
    flag <- some lowerChar
    eof
    return flag

shortflag :: Parser String
shortflag = do
    char '-'
    flag <- lowerChar
    eof
    return [flag]

flag :: Parser String
flag = do
        try (longflag)
    <|> shortflag

-- Expression of units
-- expr   = factor (. expr | / expr | ^ int | e)
-- factor = term ( ^ int | e)
-- term   = ( expr ) | unit
-- unit   = string
-- int    = ... | -1 | 0 | 2 | ...

data Expr = Mult Expr Expr
          | Div  Expr Expr
          | Expo Expr Int
          | Unit String
          deriving Show


expr :: Parser Expr
expr = do f <- factor
          do symbol "."
             e <- expr
             return (Mult f e)
            <|> do symbol "/"
                   e <- expr
                   return (Div f e)
            <|> return f

factor :: Parser Expr
factor = do t <- term
            do symbol "^"
               n <- integer
               return (Expo t n)
              <|> return t

term :: Parser Expr
term = do symbol "("
          e <- expr
          symbol ")"
          return e
          <|> do unit <- freespace $ some letterChar
                 return (Unit unit)
