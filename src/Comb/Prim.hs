{-|
Module      : Comb.Prim
Description : Primitive parsers
Copyright   : (C) Splinter Suidman, 2019
License     : AGPL-3

"Comb.Prim" defines primitive parsers.
-}
module Comb.Prim
  ( satisfy
  , many
  , many1
  , many1List
  , symbol
  , token
  , Comb.Prim.any
  , option
  , oneOf
  , oneOfSet
  , chainl
  , chainr
  , eof
  , between
  , sepBy
  , first
  , greedy
  , greedy1
  , int
  , betweenSymbols
  , parenthesised
  , bracketed
  , braced
  , quoted
    -- ** 'Char'-predicate parsers
    --
    -- | The following parsers recognise a character specified by a predicate
    -- (functions with a name starting with @is@) from "Data.Char".
  , control
  , space
  , lower
  , upper
  , alpha
  , alphaNum
  , Comb.Prim.print
  , digit
  , octDigit
  , hexDigit
  , letter
  , mark
  , number
  , punctuation
  , unicodeSymbol
  , separator
  , ascii
  , latin1
  , asciiUpper
  , asciiLower
  ) where

import           Comb.Parser
import           Comb.Stream
import           Data.Char
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Set           (Set)
import qualified Data.Set           as Set

-- | 'satisfy' @p@ returns a 'Parser' that recognises a symbol for which @p@
-- returns 'True'.
satisfy ::
     (ParserFunctor f, Stream s t)
  => (t -> Bool)
  -> Parser f s t
satisfy p =
  Parser $ \xs ->
    case uncons xs of
      Just (x, ys)
        | p x -> pure (x, ys)
      _ -> empty

-- | 'many' @p@ parses zero or more occurrences of @p@.
many :: (ParserFunctor f) => Parser f s a -> Parser f s [a]
many p = many1List p <|> pure []

-- | 'many1' @p@ parses one or more occurrences of @p@ and returns a 'NonEmpty'
-- list. For a version that returns a '[]' list, use 'many1List'.
many1 :: (ParserFunctor f) => Parser f s a -> Parser f s (NonEmpty a)
many1 p = (:|) <$> p <*> many p

-- | 'many1List' @p@ parses one or more occurrences of @p@ and returns a '[]'
-- list. For a version that returns a 'NonEmpty' list use 'many1'.
many1List :: (ParserFunctor f) => Parser f s a -> Parser f s [a]
many1List p = NonEmpty.toList <$> many1 p

-- | 'symbol' @x@ parses a symbol equal to @x@, using '==' from 'Eq' for
-- comparison.
symbol :: (ParserFunctor f, Stream s t, Eq t) => t -> Parser f s t
symbol x = satisfy (== x)

-- | 'token' @xs@ parses a list of symbols. See 'symbol' for information about
-- symbols.
token :: (ParserFunctor f, Stream s t, Eq t) => [t] -> Parser f s [t]
token xs =
  case xs of
    x:ys -> (:) <$> symbol x <*> token ys
    []   -> pure []

-- | 'any' parses any symbol.
any :: (ParserFunctor f, Stream s t) => Parser f s t
any = satisfy (const True)

-- | 'option' @p@ @d@ parses @p@, and, when @p@ fails, returns @d@.
option :: (ParserFunctor f) => Parser f s a -> a -> Parser f s a
option p d = p <|> pure d

-- | 'oneOf' @xs@ parses any element of @xs@.
--
-- Note: this is not very fast when @xs@ is large, since it uses 'elem'
-- (/O(n)/). It would be faster to use 'oneOfSet' (which has a lookup complexity
-- of /O(log n)/).
oneOf :: (ParserFunctor f, Stream s t, Eq t) => [t] -> Parser f s t
oneOf xs = satisfy (`elem` xs)

-- | 'oneOfSet' @xs@ parses any element of @xs@. 'oneOfSet' is a version of
-- 'oneOf' that uses a 'Set' instead of a list for element lookup, which is
-- often faster (/O(log n)/ for a 'Set' versus /O(n)/ for a list). To use
-- 'oneOfSet', and instance of 'Ord' is necessary.
oneOfSet :: (ParserFunctor f, Stream s t, Ord t) => Set t -> Parser f s t
oneOfSet xs = satisfy (`Set.member` xs)

-- | 'chainl' @p@ @s@ parses a list of @p@ separated by a left-associative
-- operator @s@, and collects results with the operator that @s@ returns.
--
-- >>> runParser (chainl int ((-) <$ symbol '-')) "20-10-5" :: Maybe (Int, String)
-- Just (5, "")
chainl ::
     (ParserFunctor f)
  => Parser f s a
  -> Parser f s (a -> a -> a)
  -> Parser f s a
chainl p s = foldl (flip ($)) <$> p <*> many (ap <$> s <*> p)
  where
    ap :: (a -> b -> c) -> b -> a -> c
    ap = flip
    -- Alternative definition:
    -- ap op x y = op y x

-- | 'chainr' @p@ @s@ parses a list of @p@ separated by a right-associative
-- operator @s@, and collects results with the operator that @s@ returns.
--
-- >>> runParser (chainr int ((+) <$ symbol '+')) "1+2+3" :: Maybe (Int, String)
-- Just (6, "")
--
-- Note: the operator '+' is associative (right and left), so the above example
-- could also be written with 'chainl'.
chainr ::
     (ParserFunctor f)
  => Parser f s a
  -> Parser f s (a -> a -> a)
  -> Parser f s a
chainr p s = flip (foldr ($)) <$> many (ap <$> p <*> s) <*> p
  where
    ap :: a -> (a -> b -> c) -> b -> c
    ap = flip ($)
    -- Alternative definition:
    -- ap x op = (x `op`)

-- | 'eof' returns successfully if the end of the stream has been reached.
-- Otherwise it fails.
eof :: (ParserFunctor f, Stream s t) => Parser f s ()
eof =
  Parser $ \xs ->
    case uncons xs of
      Nothing -> pure ((), xs)
      _       -> empty

-- | 'between' @open@ @close@ @p@ parses @open@, then @p@, and then @close@, and
-- returns the value of @p@.
--
-- Specialised versions of 'between' are 'betweenSymbols', 'parenthesised',
-- 'bracketed', and 'braced'.
between ::
     (ParserFunctor f, Stream s t)
  => Parser f s open
  -> Parser f s close
  -> Parser f s a
  -> Parser f s a
between open close p = open *> p <* close

-- | 'sepBy' @sep@ @p@ parses a list of @p@, separated by @sep@.
--
-- >>> runParser (sepBy (symbol ',') alpha) "a,b,c" :: Maybe ([Char], String)
-- Just ("abc", [])
sepBy ::
     (ParserFunctor f, Stream s t)
  => Parser f s sep
  -> Parser f s a
  -> Parser f s [a]
sepBy sep p = (:) <$> p <*> (many (sep *> p))

-- | 'first' @p@ only parses the first parse tree when @f ~ []@. This function
-- is used to speed up parsers that should not backtrack.
first :: Parser [] s a -> Parser [] s a
first p = Parser $ (f . runParser p)
  where
    f xs =
      case xs of
        x:_ -> [x]
        []  -> []

-- | 'greedy' is a non-backtracking version of 'many', for @f ~ []@.
greedy :: Parser [] s a -> Parser [] s [a]
greedy = first . many

-- | 'greedy1' is a non-backtracking version of 'many1', for @f ~ []@.
greedy1 :: Parser [] s a -> Parser [] s [a]
greedy1 = first . many1List

-- | 'int' parses an integer and returns an 'Int'. The integer may consist only
-- of digits (parsed by 'digit'); leading zeroes are allowed.
int :: (ParserFunctor f, Stream s Char) => Parser f s Int
int = foldl (\a b -> a * 10 + digitToInt b) 0 <$> many1 digit

-- | 'betweenSymbols' is a specialised function of 'between'; instead of two
-- parsers, it accepts to symbols that surround the parser.
betweenSymbols ::
     (ParserFunctor f, Stream s t, Eq t)
  => t
  -> t
  -> Parser f s a
  -> Parser f s a
betweenSymbols open close = between (symbol open) (symbol close)

-- | Parse an expression between two parentheses (@(@ and @)@).
--
-- >>> runParser (parenthesised (many alpha)) "(abc)" :: Maybe (String, String)
-- Just ("abc","")
parenthesised ::
     (ParserFunctor f, Stream s Char)
  => Parser f s a
  -> Parser f s a
parenthesised = betweenSymbols '(' ')'

-- | Parse an expression between two straight brackets (@[@ and @]@).
--
-- >>> runParser (bracketed (many alpha)) "[abc]" :: Maybe (String, String)
-- Just ("abc","")
bracketed :: (ParserFunctor f, Stream s Char) => Parser f s a -> Parser f s a
bracketed = betweenSymbols '[' ']'

-- | Parse an expression between two curly braces (@{@ and @}@).
--
-- >>> runParser (braced (many alpha)) "{abc}" :: Maybe (String, String)
-- Just ("abc","")
braced :: (ParserFunctor f, Stream s Char) => Parser f s a -> Parser f s a
braced = betweenSymbols '{' '}'

-- | Parse an expression between two straight double quotes (@"@).
--
-- >>> runParser (quoted (many alpha)) "\"abc\"" :: Maybe (String, String)
-- Just ("abc","")
quoted :: (ParserFunctor f, Stream s Char) => Parser f s a -> Parser f s a
quoted = betweenSymbols '"' '"'

-- | Parse a character that is recognised by 'isControl'.
control :: (ParserFunctor f, Stream s Char) => Parser f s Char
control = satisfy isControl

-- | Parse a character that is recognised by 'isSpace'.
space :: (ParserFunctor f, Stream s Char) => Parser f s Char
space = satisfy isSpace

-- | Parse a character that is recognised by 'isLower'.
lower :: (ParserFunctor f, Stream s Char) => Parser f s Char
lower = satisfy isLower

-- | Parse a character that is recognised by 'isUpper'.
upper :: (ParserFunctor f, Stream s Char) => Parser f s Char
upper = satisfy isUpper

-- | Parse a character that is recognised by 'isAlpha'.
alpha :: (ParserFunctor f, Stream s Char) => Parser f s Char
alpha = satisfy isAlpha

-- | Parse a character that is recognised by 'isAlphaNum'.
alphaNum :: (ParserFunctor f, Stream s Char) => Parser f s Char
alphaNum = satisfy isAlphaNum

-- | Parse a character that is recognised by 'isPrint'.
print :: (ParserFunctor f, Stream s Char) => Parser f s Char
print = satisfy isPrint

-- | Parse a character that is recognised by 'isDigit'.
digit :: (ParserFunctor f, Stream s Char) => Parser f s Char
digit = satisfy isDigit

-- | Parse a character that is recognised by 'isOctDigit'.
octDigit :: (ParserFunctor f, Stream s Char) => Parser f s Char
octDigit = satisfy isOctDigit

-- | Parse a character that is recognised by 'isHexDigit'.
hexDigit :: (ParserFunctor f, Stream s Char) => Parser f s Char
hexDigit = satisfy isHexDigit

-- | Parse a character that is recognised by 'isLetter'.
letter :: (ParserFunctor f, Stream s Char) => Parser f s Char
letter = satisfy isLetter

-- | Parse a character that is recognised by 'isMark'.
mark :: (ParserFunctor f, Stream s Char) => Parser f s Char
mark = satisfy isMark

-- | Parse a character that is recognised by 'isNumber'.
number :: (ParserFunctor f, Stream s Char) => Parser f s Char
number = satisfy isNumber

-- | Parse a character that is recognised by 'isPunctuation'.
punctuation :: (ParserFunctor f, Stream s Char) => Parser f s Char
punctuation = satisfy isPunctuation

-- TODO: name clash with 'symbol' when using the default naming convention for
-- these functions.
-- | Parse a character that is recognised by 'isSymbol'.
unicodeSymbol :: (ParserFunctor f, Stream s Char) => Parser f s Char
unicodeSymbol = satisfy isSymbol

-- | Parse a character that is recognised by 'isSeparator'.
separator :: (ParserFunctor f, Stream s Char) => Parser f s Char
separator = satisfy isSeparator

-- | Parse a character that is recognised by 'isAscii'.
ascii :: (ParserFunctor f, Stream s Char) => Parser f s Char
ascii = satisfy isAscii

-- | Parse a character that is recognised by 'isLatin1'.
latin1 :: (ParserFunctor f, Stream s Char) => Parser f s Char
latin1 = satisfy isLatin1

-- | Parse a character that is recognised by 'isAsciiUpper'.
asciiUpper :: (ParserFunctor f, Stream s Char) => Parser f s Char
asciiUpper = satisfy isAsciiUpper

-- | Parse a character that is recognised by 'isAsciiLower'.
asciiLower :: (ParserFunctor f, Stream s Char) => Parser f s Char
asciiLower = satisfy isAsciiLower
