module Arithmetic
  ( Expr (..)
  , parseExpr
  , evalExpr
  ) where

import Comb

data Expr = Con Int
          | Expr :+: Expr
          | Expr :-: Expr
          | Expr :*: Expr
          | Expr :/: Expr
          deriving (Eq, Show)

-- Parse a factor.
fact :: Parser Char Expr
fact =  Con <$> int
    <|> parenthesised expr

-- Parse a term.
term :: Parser Char Expr
term = chainl fact ( symbol '*' `replaceWith` (:*:)
                 <|> symbol '/' `replaceWith` (:/:))

-- Parse an expression.
expr :: Parser Char Expr
expr = chainl term ( symbol '+' `replaceWith` (:+:)
                 <|> symbol '-' `replaceWith` (:-:))

parseExpr :: String -> Expr
parseExpr s = fst . head $ parse (first expr) s

factInt :: Parser Char Int
factInt =  int
       <|> parenthesised exprInt

termInt :: Parser Char Int
termInt = chainl factInt ( symbol '*' `replaceWith` (*)
                       <|> symbol '/' `replaceWith` div)

exprInt :: Parser Char Int
exprInt = chainl termInt ( symbol '+' `replaceWith` (+)
                       <|> symbol '-' `replaceWith` (-))

evalExpr :: String -> Int
evalExpr s = fst . head $ parse (first exprInt) s
