-- |'Parser' contains the parser type: 'Parser', and instances for some
-- (monadic) classes.
module Parser
  ( Parser (..)
  , (<|>) -- Export Alternative's (<|>), to minimalise the number of imports.
  ) where

import Control.Applicative (Alternative (..))

-- |The type of a parser. 's' is the symbol's type, 'a' the return type.
-- 'parse' is the parse function, which can be used like this: @parse p input@,
-- where @p@ is a parser, and @input@ is the input string of symbols.
newtype Parser s a = Parser { parse :: [s] -> [(a, [s])] }

parserMap :: (a -> b) -> Parser s a -> Parser s b
parserMap f p = Parser parser
  where
    parser xs = fmap (\(x, ys) -> (f x, ys)) $ parse p xs

parserReturn :: a -> Parser s a
parserReturn x = Parser $ \xs -> [(x, xs)]

parserBind :: Parser s a -> (a -> Parser s b) -> Parser s b
parserBind p f = Parser $ \xs ->
  concatMap (\(x, ys) -> parse (f x) ys) $ parse p xs

parserCombine :: Parser s (a -> b) -> Parser s a -> Parser s b
parserCombine p q = Parser $ \xs ->
  [ (f x, zs)
  | (f, ys) <- parse p xs
  , (x, zs) <- parse q ys
  ]

parserAlternative :: Parser s a -> Parser s a -> Parser s a
parserAlternative p q = Parser $ \xs -> parse p xs ++ parse q xs

instance Functor (Parser s) where
  fmap = parserMap

instance Applicative (Parser s) where
  pure = parserReturn
  (<*>) = parserCombine

instance Alternative (Parser s) where
  empty = Parser $ \xs -> []
  (<|>) = parserAlternative

instance Monad (Parser s) where
  return = pure
  (>>=) = parserBind
