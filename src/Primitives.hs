-- |'Primitives' exports some primitive parser functions.
module Primitives where

import Data.Char (isDigit, isSpace, isLetter, isLower, isUpper)
import Parser

-- |Parse an 's' that satisfies the predicate.
--
-- Grammar: \(\mathit{satisfy\ p} \rightarrow \{x\ |\ p(x)\}\)
satisfy :: (s -> Bool) -> Parser s s
satisfy p = Parser parser
  where
    parser [] = []
    parser (x:xs)
      | p x       = [(x, xs)]
      | otherwise = []

-- |Parse an 's' that equals the argument.
symbol :: Eq s => s -> Parser s s
symbol a = satisfy (== a)

-- |Parse a string of 's' that equals the argument.
token :: Eq s => [s] -> Parser s [s]
token k = Parser parser
  where
    n = length k
    parser xs
      | k == take n xs = [(k, drop n xs)]
      | otherwise      = []

-- |Parse zero or more occurrences of the parser.
--
-- Grammar: \(\mathit{many\ p} \rightarrow \mathit{p}^*\).
many :: Parser s a -> Parser s [a]
many p =  do x  <- p      -- parse trees of p
             ys <- many p -- list of parse trees of many p
             return $ x : ys
      <|> return []

-- |Parse any element of type 's'.
any :: Parser s s
any = satisfy (const True)

-- |Parse one or more occurrences of the parser.
--
-- Grammar: \(\mathit{many\ p} \rightarrow \mathit{p}^+\)
many1 :: Parser s a -> Parser s [a]
many1 p = do x  <- p
             ys <- many p
             return $ x : ys

-- |Parse an expression or return a constant value when the parser fails.
option :: Parser s a -> a -> Parser s a
option p d = p <|> return d

-- |Replace the returned value of the parser with a constant.
replace :: b -> Parser s a -> Parser s b
replace x p = const x <$> p

-- |Same as 'replace', but with its arguments flipped, which is useful for,
-- for example: @symbol \'+\' \`replaceWith\` (+)@.
replaceWith :: Parser s a -> b -> Parser s b
replaceWith = flip replace

-- |An element of the given list.
--
-- Grammar: \(\mathit{oneOf\ xs} \rightarrow \{\mathit{x}\ |\ \mathit{x} \in \mathit{xs}\}\)
oneOf :: Eq s => [s] -> Parser s s
oneOf xs = satisfy (`elem` xs)

-- |Parse an expression with the parser @p@ between two (other) expressions,
-- returning the value of @p@.
--
-- Grammar: \(\mathit{between\ open\ close\ p} \rightarrow \mathit{open\ p\ close}\)
between :: Parser s open -> Parser s close -> Parser s a -> Parser s a
between open close p = do open
                          x <- p
                          close
                          return x

-- SepBy -> P (Sep P)*
-- |Parse a list of parser @p@, seperated by parser @sep@.
--
-- Grammar: \(\mathit{sepBy\ sep\ p} \rightarrow \mathit{p}\ (\mathit{sep\ p})^*\)
sepBy :: Parser s sep -> Parser s a -> Parser s [a]
sepBy sep p = do x    <- p
                 rest <- many $
                   do sep
                      p
                 return $ x : rest

-- |Only evaluate the first parse tree, which is the left-most derivation.
first :: Parser s a -> Parser s a
first p = Parser $ f <$> parse p
  where
    f []    = []
    f (x:_) = [x]

-- |'first' applied to 'many'; this takes only as much of a parser @p@ as
-- possible, and does not evaluate the other parse trees.
greedy :: Parser s a -> Parser s [a]
greedy = first . many

-- |'first' applied to 'many1'; this takes only as much of a parser @p@ as
-- possible, and does not evaluate the other parse trees.
greedy1 :: Parser s a -> Parser s [a]
greedy1 = first . many1

-- |Parse a digit, a 'Char' that satisfies 'isDigit'.
digit :: Parser Char Char
digit = satisfy isDigit

-- |Parse any whitespace, a 'Char' that satisfies 'isSpace'.
space :: Parser Char Char
space = satisfy isSpace

-- |Parse a letter, a 'Char' that satisfies 'isLetter'.
letter :: Parser Char Char
letter = satisfy isLetter

-- |Parse a lowercase 'Char', which satisfies 'isLower'.
lowercase :: Parser Char Char
lowercase = satisfy isLower

-- |Parse an uppercase 'Char', which satisfies 'isUpper'.
uppercase :: Parser Char Char
uppercase = satisfy isUpper

-- |Parse an 'Int', consisting of only digits (no negative sign, nor exponents, etc.).
int :: Parser Char Int
int = read <$> many1 digit

-- |Parse an 'Integer', consisting of only digits (no negative sign, nor exponents, etc.).
integer :: Parser Char Integer
integer = read <$> many1 digit

-- |Parse an expression between parentheses (@'('@ and @')'@).
parenthesised :: Parser Char a -> Parser Char a
parenthesised = between (symbol '(') (symbol ')')

-- |Parse an expression between brackets (@'['@ and @']'@).
bracketed :: Parser Char a -> Parser Char a
bracketed = between (symbol '[') (symbol ']')

-- |Parse an expression between brackets (@'{'@ and @'}'@).
braced :: Parser Char a -> Parser Char a
braced = between (symbol '{') (symbol '}')

-- |Parse @many1 expr@ seperated by @op@, @foldr@'d by the operation that the
-- parser @op@ returns.
--
-- Example:
--
-- @
--   data Expr = Con Int
--             | Expr :+: Expr
--
--   parse (first \$ chainr (Con \<\$\> int) (symbol \'+\' \`replaceWith\` (:+:))) "1+2+3"
--     ==
--   [(Con 1 :+: (Con 2 :+: Con 3), "")]
-- @
chainr :: Parser s a -> Parser s (a -> a -> a) -> Parser s a
chainr pe po =
  do rest <- many $
       do e <- pe
          o <- po
          return (e `o`)
     expr <- pe
     return $ foldr ($) expr rest

-- |Parse @many1 expr@ seperated by @op@, @foldl@'d by the operation that the
-- parser @op@ returns.
--
-- Example:
--
-- @
--   data Expr = Con Int
--             | Expr :+: Expr
--
--   parse (first \$ chainl (Con \<\$\> int) (symbol \'+\' \`replaceWith\` (:+:))) "1+2+3"
--     ==
--   [((Con 1 :+: Con 2) :+: Con 3, "")]
-- @
chainl :: Parser s a -> Parser s (a -> a -> a) -> Parser s a
chainl pe po =
  do expr <- pe
     rest <- many $
       do o <- po
          e <- pe
          return (`o` e)
     return $ foldl (flip ($)) expr rest
