{-# LANGUAGE OverloadedStrings #-}

module Parsers where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

integer :: Parser Integer
integer = lexeme L.decimal

lowerCaseAlpha :: Parser Char
lowerCaseAlpha = do oneOf ['a'..'z']

longflag :: Parser String
longflag = do
    string "--"
    flag <- some lowerCaseAlpha
    eof
    return flag

shortflag :: Parser String
shortflag = do
    char '-'
    flag <- lowerCaseAlpha
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
          | Expo Expr Integer
          | Unit String
          deriving Show


expr :: Parser Expr
expr = do f <- factor
          do char '.'
             e <- expr
             return (Mult f e)
            <|> do char '/'
                   e <- expr
                   return (Div f e)
            -- <|> do char '^'
            --        n <- integer
            --        return (Expo f n)
            <|> return f

factor :: Parser Expr
factor = do t <- term
            do char '^'
               n <- integer
               return (Expo t n)
              <|> return t

term :: Parser Expr
term = do char '('
          e <- expr
          char ')'
          return e
          <|> do str <- some lowerCaseAlpha
                 return (Unit str)
