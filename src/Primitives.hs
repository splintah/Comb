-- | 'Primitives' exports some primitive parser functions.
module Primitives where

import Control.Applicative
  ( Alternative (..)
  )
import Control.Monad
  ( MonadPlus
  )
import Data.Char
  ( isDigit
  , isSpace
  , isLetter
  , isLower
  , isUpper
  )
import Parser

-- | Parse an 's' that satisfies the predicate.
--
-- Grammar: \(\mathit{satisfy\ p} \rightarrow \{x\ |\ p(x)\}\)
satisfy :: (MonadPlus m) => (s -> Bool) -> Parser s m s
satisfy p = Parser parser
  where
    parser [] = empty
    parser (x:xs)
      | p x       = return (x, xs)
      | otherwise = empty

-- | Parse an 's' that equals the argument.
symbol :: (MonadPlus m, Eq s) => s -> Parser s m s
symbol a = satisfy (== a)

-- | Parse a string of 's' that equals the argument.
token :: (MonadPlus m, Eq s) => [s] -> Parser s m [s]
token k = Parser parser
  where
    n = length k
    parser xs
      | k == take n xs = return (k, drop n xs)
      | otherwise      = empty

-- | Parse zero or more occurrences of the parser.
--
-- Grammar: \(\mathit{many\ p} \rightarrow \mathit{p}^*\).
many :: (MonadPlus m) => Parser s m a -> Parser s m [a]
many p =  do x  <- p
             ys <- Primitives.many p
             return $ x : ys
      <|> return empty

-- | Parse any element of type 's'.
any :: (MonadPlus m) => Parser s m s
any = satisfy (const True)

-- | Parse one or more occurrences of the parser.
--
-- Grammar: \(\mathit{many\ p} \rightarrow \mathit{p}^+\)
many1 :: (MonadPlus m) => Parser s m a -> Parser s m [a]
many1 p = do x  <- p
             ys <- Primitives.many p
             return $ x : ys

-- | Parse an expression or return a constant value when the parser fails.
option :: (MonadPlus m) => Parser s m a -> a -> Parser s m a
option p d = p <|> return d

-- | Replace the returned value of the parser with a constant.
replace :: (Monad m) => b -> Parser s m a -> Parser s m b
replace x p = const x <$> p

-- | Same as 'replace', but with its arguments flipped, which is useful for,
-- for example: @symbol \'+\' \`replaceWith\` (+)@.
replaceWith :: (Monad m) => Parser s m a -> b -> Parser s m b
replaceWith = flip replace

-- | An element of the given list.
--
-- Grammar: \(\mathit{oneOf\ xs} \rightarrow \{\mathit{x}\ |\ \mathit{x} \in \mathit{xs}\}\)
oneOf :: (MonadPlus m, Eq s) => [s] -> Parser s m s
oneOf xs = satisfy (`elem` xs)

-- | Parse the end of file. Returns @()@ successfully when the end of file is
-- reached, and fails otherwise.
eof :: (MonadPlus m) => Parser s m ()
eof = Parser parser
  where
    parser [] = return ((), [])
    parser _  = empty

-- | Parse an expression with the parser @p@ between two (other) expressions,
-- returning the value of @p@.
--
-- Grammar: \(\mathit{between\ open\ close\ p} \rightarrow \mathit{open\ p\ close}\)
between :: (Monad m) => Parser s m close -> Parser s m open -> Parser s m a -> Parser s m a
between open close p = do open
                          x <- p
                          close
                          return x

-- | Parse a list of parser @p@, seperated by parser @sep@.
--
-- Grammar: \(\mathit{sepBy\ sep\ p} \rightarrow \mathit{p}\ (\mathit{sep\ p})^*\)
sepBy :: (MonadPlus m) => Parser s m sep -> Parser s m a -> Parser s m [a]
sepBy sep p = do x    <- p
                 rest <- Primitives.many $
                   do sep
                      p
                 return $ x : rest

-- | Only evaluate the first parse tree, which is the left-most derivation.
first :: Parser s [] a -> Parser s [] a
first p = Parser $ f <$> parse p
  where
    f []    = []
    f (x:_) = [x]

-- | 'first' applied to 'many'; this takes only as much of a parser @p@ as
-- possible, and does not evaluate the other parse trees.
greedy :: Parser s [] a -> Parser s [] [a]
greedy = first . Primitives.many

-- | 'first' applied to 'many1'; this takes only as much of a parser @p@ as
-- possible, and does not evaluate the other parse trees.
greedy1 :: Parser s [] a -> Parser s [] [a]
greedy1 = first . many1

-- | Parse a digit, a 'Char' that satisfies 'isDigit'.
digit :: (MonadPlus m) => Parser Char m Char
digit = satisfy isDigit

-- | Parse any whitespace, a 'Char' that satisfies 'isSpace'.
space :: (MonadPlus m) => Parser Char m Char
space = satisfy isSpace

-- | Parse a letter, a 'Char' that satisfies 'isLetter'.
letter :: (MonadPlus m) => Parser Char m Char
letter = satisfy isLetter

-- | Parse a lowercase 'Char', which satisfies 'isLower'.
lowercase :: (MonadPlus m) => Parser Char m Char
lowercase = satisfy isLower

-- | Parse an uppercase 'Char', which satisfies 'isUpper'.
uppercase :: (MonadPlus m) => Parser Char m Char
uppercase = satisfy isUpper

-- | Parse an 'Int', consisting of an optional sign ('+' or '-'; without a sign
-- '+' is inferred) and 'many1' digits.
int :: (MonadPlus m) => Parser Char m Int
int = do sign <- option (   symbol '+' `replaceWith` (+)
                        <|> symbol '-' `replaceWith` (-))
                        (+)
         digs <- many1 digit
         return $ 0 `sign` (read digs)

-- | Parse an 'Integer', consisting of an optional sign ('+' or '-'; without a
-- sign '+' is inferred) and 'many1' digits.
integer :: (MonadPlus m) => Parser Char m Integer
integer = do sign <- option (   symbol '+' `replaceWith` (+)
                            <|> symbol '-' `replaceWith` (-))
                            (+)
             digs <- many1 digit
             return $ 0 `sign` (read digs)

-- | Parse an expression between parentheses (@'('@ and @')'@).
parenthesised :: (MonadPlus m) => Parser Char m a -> Parser Char m a
parenthesised = between (symbol '(') (symbol ')')

-- | Parse an expression between brackets (@'['@ and @']'@).
bracketed :: (MonadPlus m) => Parser Char m a -> Parser Char m a
bracketed = between (symbol '[') (symbol ']')

-- | Parse an expression between brackets (@'{'@ and @'}'@).
braced :: (MonadPlus m) => Parser Char m a -> Parser Char m a
braced = between (symbol '{') (symbol '}')

-- | Parse @many1 expr@ seperated by @op@, @foldr@'d by the operation that the
-- parser @op@ returns.
--
-- ==== __Example__
--
-- @
--   data Expr = Con Int
--             | Expr :+: Expr
--
--   'parse' ('first' \$ 'chainr' (Con \<\$\> 'int') ('symbol' \'+\' \`replaceWith\` (:+:))) "1+2+3"
--     ==
--   [(Con 1 :+: (Con 2 :+: Con 3), "")]
-- @
chainr :: (MonadPlus m) => Parser s m a -> Parser s m (a -> a -> a) -> Parser s m a
chainr pe po =
  do rest <- Primitives.many $
       do e <- pe
          o <- po
          return (e `o`)
     expr <- pe
     return $ foldr ($) expr rest

-- | Parse @many1 expr@ seperated by @op@, @foldl@'d by the operation that the
-- parser @op@ returns.
--
-- ==== __Example__
--
-- @
--   data Expr = Con Int
--             | Expr :+: Expr
--
--   'parse' ('first' \$ 'chainl' (Con \<\$\> 'int') ('symbol' \'+\' \`replaceWith\` (:+:))) "1+2+3"
--     ==
--   [((Con 1 :+: Con 2) :+: Con 3, "")]
-- @
chainl :: (MonadPlus m) => Parser s m a -> Parser s m (a -> a -> a) -> Parser s m a
chainl pe po =
  do expr <- pe
     rest <- Primitives.many $
       do o <- po
          e <- pe
          return (`o` e)
     return $ foldl (flip ($)) expr rest

-- | Only return a successful value when the input is consumed.
complete :: (MonadPlus m) => Parser s m a -> Parser s m a
complete p = Parser $ \xs ->
  do x@(_, ys) <- parse p xs
     if null ys
        then return x
        else empty
