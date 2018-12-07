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
fact :: Parser Char Maybe Expr
fact =  Con <$> int
    <|> parenthesised expr

-- Parse a term.
term :: Parser Char Maybe Expr
term = chainl fact ( symbol '*' `replaceWith` (:*:)
                 <|> symbol '/' `replaceWith` (:/:))

-- Parse an expression.
expr :: Parser Char Maybe Expr
expr = chainl term ( symbol '+' `replaceWith` (:+:)
                 <|> symbol '-' `replaceWith` (:-:))

parseExpr :: String -> Maybe Expr
parseExpr s = fmap fst $ parse expr s

factInt :: Parser Char Maybe Int
factInt =  int
       <|> parenthesised exprInt

termInt :: Parser Char Maybe Int
termInt = chainl factInt ( symbol '*' `replaceWith` (*)
                       <|> symbol '/' `replaceWith` div)

exprInt :: Parser Char Maybe Int
exprInt = chainl termInt ( symbol '+' `replaceWith` (+)
                       <|> symbol '-' `replaceWith` (-))

evalExpr :: String -> Maybe Int
evalExpr = fmap fst . parse (complete exprInt)
