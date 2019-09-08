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
  , greedy1List
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
     (ParserMonad m, Stream s t)
  => (t -> Bool)
  -> Parser m s t
satisfy p =
  Parser $ \xs ->
    case uncons xs of
      Just (x, ys)
        | p x -> pure (x, ys)
      _ -> empty

-- | 'many' @p@ parses zero or more occurrences of @p@.
many :: (ParserMonad m) => Parser m s a -> Parser m s [a]
many p = many1List p <|> pure []

-- | 'many1' @p@ parses one or more occurrences of @p@ and returns a 'NonEmpty'
-- list. For a version that returns a '[]' list, use 'many1List'.
many1 :: (ParserMonad m) => Parser m s a -> Parser m s (NonEmpty a)
many1 p = (:|) <$> p <*> many p

-- | 'many1List' @p@ parses one or more occurrences of @p@ and returns a '[]'
-- list. For a version that returns a 'NonEmpty' list use 'many1'.
many1List :: (ParserMonad m) => Parser m s a -> Parser m s [a]
many1List p = NonEmpty.toList <$> many1 p

-- | 'symbol' @x@ parses a symbol equal to @x@, using '==' from 'Eq' for
-- comparison.
symbol :: (ParserMonad m, Stream s t, Eq t) => t -> Parser m s t
symbol x = satisfy (== x)

-- | 'token' @xs@ parses a list of symbols. See 'symbol' for information about
-- symbols.
token :: (ParserMonad m, Stream s t, Eq t) => [t] -> Parser m s [t]
token xs =
  case xs of
    x:ys -> (:) <$> symbol x <*> token ys
    []   -> pure []

-- | 'any' parses any symbol.
any :: (ParserMonad m, Stream s t) => Parser m s t
any = satisfy (const True)

-- | 'option' @p@ @d@ parses @p@, and, when @p@ fails, returns @d@.
option :: (ParserMonad m) => Parser m s a -> a -> Parser m s a
option p d = p <|> pure d

-- | 'oneOf' @xs@ parses any element of @xs@.
--
-- Note: this is not very fast when @xs@ is large, since it uses 'elem'
-- (/O(n)/). It would be faster to use 'oneOfSet' (which has a lookup complexity
-- of /O(log n)/).
oneOf :: (ParserMonad m, Stream s t, Eq t) => [t] -> Parser m s t
oneOf xs = satisfy (`elem` xs)

-- | 'oneOfSet' @xs@ parses any element of @xs@. 'oneOfSet' is a version of
-- 'oneOf' that uses a 'Set' instead of a list for element lookup, which is
-- often faster (/O(log n)/ for a 'Set' versus /O(n)/ for a list). To use
-- 'oneOfSet', and instance of 'Ord' is necessary.
oneOfSet :: (ParserMonad m, Stream s t, Ord t) => Set t -> Parser m s t
oneOfSet xs = satisfy (`Set.member` xs)

-- | 'chainl' @p@ @s@ parses a list of @p@ separated by a left-associative
-- operator @s@, and collects results with the operator that @s@ returns.
--
-- >>> runParser (chainl int ((-) <$ symbol '-')) "20-10-5" :: Maybe (Int, String)
-- Just (5, "")
chainl ::
     (ParserMonad m)
  => Parser m s a
  -> Parser m s (a -> a -> a)
  -> Parser m s a
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
     (ParserMonad m)
  => Parser m s a
  -> Parser m s (a -> a -> a)
  -> Parser m s a
chainr p s = flip (foldr ($)) <$> many (ap <$> p <*> s) <*> p
  where
    ap :: a -> (a -> b -> c) -> b -> c
    ap = flip ($)
    -- Alternative definition:
    -- ap x op = (x `op`)

-- | 'eof' returns successfully if the end of the stream has been reached.
-- Otherwise it fails.
eof :: (ParserMonad m, Stream s t) => Parser m s ()
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
     (ParserMonad m, Stream s t)
  => Parser m s open
  -> Parser m s close
  -> Parser m s a
  -> Parser m s a
between open close p = open *> p <* close

-- | 'sepBy' @p@ @sep@ parses a list of @p@, separated by @sep@.
--
-- >>> runParser (sepBy alpha (symbol ',')) "a,b,c" :: Maybe ([Char], String)
-- Just ("abc", [])
sepBy ::
     (ParserMonad m, Stream s t)
  => Parser m s a
  -> Parser m s sep
  -> Parser m s [a]
sepBy p sep = (:) <$> p <*> (many (sep *> p))

-- | 'first' @p@ only parses the first parse tree when @m ~ []@. This function
-- is used to speed up parsers that should not backtrack.
first :: Parser [] s a -> Parser [] s a
first p = Parser $ (f . runParser p)
  where
    f xs =
      case xs of
        x:_ -> [x]
        []  -> []

-- | 'greedy' is a non-backtracking version of 'many', for @GreedyAlternative
-- m@. The greedy parsers are useful for parsing identifiers or whitespace, for
-- example. In most grammars, an identifier like @"greedy"@ can only be parsed
-- as the full identifier, and not as the combination of subsequences like
-- @"gre"@ and @"edy"@.
greedy :: (ParserMonad m, GreedyAlternative m) => Parser m s a -> Parser m s [a]
greedy p = greedy1List p <||> pure []

-- | 'greedy1' is a non-backtracking version of 'many1', for @GreedyAlternative
-- m@. See 'greedy' for more information.
greedy1 :: (ParserMonad m, GreedyAlternative m) => Parser m s a -> Parser m s (NonEmpty a)
greedy1 p = (:|) <$> p <*> greedy p

-- | 'greedy1List' is a non-backtracking version of 'many1List', for
-- @GreedyAlternative m@. See 'greedy' for more information.
greedy1List :: (ParserMonad m, GreedyAlternative m) => Parser m s a -> Parser m s [a]
greedy1List p = NonEmpty.toList <$> greedy1 p

-- | 'int' parses an integer and returns an 'Int'. The integer may consist only
-- of digits (parsed by 'digit'); leading zeroes are allowed.
int :: (ParserMonad m, Stream s Char) => Parser m s Int
int = foldl (\a b -> a * 10 + digitToInt b) 0 <$> many1 digit

-- | 'betweenSymbols' is a specialised function of 'between'; instead of two
-- parsers, it accepts to symbols that surround the parser.
betweenSymbols ::
     (ParserMonad m, Stream s t, Eq t)
  => t
  -> t
  -> Parser m s a
  -> Parser m s a
betweenSymbols open close = between (symbol open) (symbol close)

-- | Parse an expression between two parentheses (@(@ and @)@).
--
-- >>> runParser (parenthesised (many alpha)) "(abc)" :: Maybe (String, String)
-- Just ("abc","")
parenthesised ::
     (ParserMonad m, Stream s Char)
  => Parser m s a
  -> Parser m s a
parenthesised = betweenSymbols '(' ')'

-- | Parse an expression between two straight brackets (@[@ and @]@).
--
-- >>> runParser (bracketed (many alpha)) "[abc]" :: Maybe (String, String)
-- Just ("abc","")
bracketed :: (ParserMonad m, Stream s Char) => Parser m s a -> Parser m s a
bracketed = betweenSymbols '[' ']'

-- | Parse an expression between two curly braces (@{@ and @}@).
--
-- >>> runParser (braced (many alpha)) "{abc}" :: Maybe (String, String)
-- Just ("abc","")
braced :: (ParserMonad m, Stream s Char) => Parser m s a -> Parser m s a
braced = betweenSymbols '{' '}'

-- | Parse an expression between two straight double quotes (@"@).
--
-- >>> runParser (quoted (many alpha)) "\"abc\"" :: Maybe (String, String)
-- Just ("abc","")
quoted :: (ParserMonad m, Stream s Char) => Parser m s a -> Parser m s a
quoted = betweenSymbols '"' '"'

-- | Parse a character that is recognised by 'isControl'.
control :: (ParserMonad m, Stream s Char) => Parser m s Char
control = satisfy isControl

-- | Parse a character that is recognised by 'isSpace'.
space :: (ParserMonad m, Stream s Char) => Parser m s Char
space = satisfy isSpace

-- | Parse a character that is recognised by 'isLower'.
lower :: (ParserMonad m, Stream s Char) => Parser m s Char
lower = satisfy isLower

-- | Parse a character that is recognised by 'isUpper'.
upper :: (ParserMonad m, Stream s Char) => Parser m s Char
upper = satisfy isUpper

-- | Parse a character that is recognised by 'isAlpha'.
alpha :: (ParserMonad m, Stream s Char) => Parser m s Char
alpha = satisfy isAlpha

-- | Parse a character that is recognised by 'isAlphaNum'.
alphaNum :: (ParserMonad m, Stream s Char) => Parser m s Char
alphaNum = satisfy isAlphaNum

-- | Parse a character that is recognised by 'isPrint'.
print :: (ParserMonad m, Stream s Char) => Parser m s Char
print = satisfy isPrint

-- | Parse a character that is recognised by 'isDigit'.
digit :: (ParserMonad m, Stream s Char) => Parser m s Char
digit = satisfy isDigit

-- | Parse a character that is recognised by 'isOctDigit'.
octDigit :: (ParserMonad m, Stream s Char) => Parser m s Char
octDigit = satisfy isOctDigit

-- | Parse a character that is recognised by 'isHexDigit'.
hexDigit :: (ParserMonad m, Stream s Char) => Parser m s Char
hexDigit = satisfy isHexDigit

-- | Parse a character that is recognised by 'isLetter'.
letter :: (ParserMonad m, Stream s Char) => Parser m s Char
letter = satisfy isLetter

-- | Parse a character that is recognised by 'isMark'.
mark :: (ParserMonad m, Stream s Char) => Parser m s Char
mark = satisfy isMark

-- | Parse a character that is recognised by 'isNumber'.
number :: (ParserMonad m, Stream s Char) => Parser m s Char
number = satisfy isNumber

-- | Parse a character that is recognised by 'isPunctuation'.
punctuation :: (ParserMonad m, Stream s Char) => Parser m s Char
punctuation = satisfy isPunctuation

-- TODO: name clash with 'symbol' when using the default naming convention for
-- these functions.
-- | Parse a character that is recognised by 'isSymbol'.
unicodeSymbol :: (ParserMonad m, Stream s Char) => Parser m s Char
unicodeSymbol = satisfy isSymbol

-- | Parse a character that is recognised by 'isSeparator'.
separator :: (ParserMonad m, Stream s Char) => Parser m s Char
separator = satisfy isSeparator

-- | Parse a character that is recognised by 'isAscii'.
ascii :: (ParserMonad m, Stream s Char) => Parser m s Char
ascii = satisfy isAscii

-- | Parse a character that is recognised by 'isLatin1'.
latin1 :: (ParserMonad m, Stream s Char) => Parser m s Char
latin1 = satisfy isLatin1

-- | Parse a character that is recognised by 'isAsciiUpper'.
asciiUpper :: (ParserMonad m, Stream s Char) => Parser m s Char
asciiUpper = satisfy isAsciiUpper

-- | Parse a character that is recognised by 'isAsciiLower'.
asciiLower :: (ParserMonad m, Stream s Char) => Parser m s Char
asciiLower = satisfy isAsciiLower
