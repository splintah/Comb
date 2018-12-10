-- | 'Parser' contains the parser type: 'Parser', and instances for some
-- (monadic) classes.
module Parser
  ( Parser (..)
  , (<|>) -- Export Alternative's (<|>), to minimalise the number of imports.
  ) where

import Control.Applicative
  ( Alternative (..)
  , Applicative (..)
  )
import Control.Monad
  ( MonadPlus (..)
  )

-- | The type of a parser. 's' is the symbol's type, 'a' the return type, 'm' is
-- the 'Monad' type. 'parse' is the parse function, which can be used like this:
-- @parse p input@, where @p@ is a parser, and @input@ is the input string of
-- symbols.
newtype Parser s m a = Parser { parse :: [s] -> m (a, [s]) }

parserMap :: (Monad m, Functor m) => (a -> b) -> Parser s m a -> Parser s m b
parserMap f p = Parser $ \xs ->
  do (x, ys) <- parse p xs
     return (f x, ys)

parserReturn :: (Monad m) => a -> Parser s m a
parserReturn x = Parser $ \xs -> return (x, xs)

parserBind :: (Monad m) => Parser s m a -> (a -> Parser s m b) -> Parser s m b
parserBind p f = Parser $ \xs ->
  do (x, ys) <- parse p xs
     parse (f x) ys

parserCombine :: (Monad m) => Parser s m (a -> b) -> Parser s m a -> Parser s m b
parserCombine p q = Parser $ \xs ->
  do (f, ys) <- parse p xs
     (x, zs) <- parse q ys
     return (f x, zs)

parserAlternative :: (Monad m, Alternative m) => Parser s m a -> Parser s m a -> Parser s m a
parserAlternative p q = Parser $ \xs -> parse p xs <|> parse q xs

instance (Monad m) => Functor (Parser s m) where
  fmap = parserMap

instance (Monad m) => Applicative (Parser s m) where
  pure = parserReturn
  (<*>) = parserCombine

instance (Monad m, Alternative m) => Alternative (Parser s m) where
  empty = Parser $ \xs -> empty
  (<|>) = parserAlternative

instance (Monad m) => Monad (Parser s m) where
  return = pure
  (>>=) = parserBind

instance (MonadPlus m) => MonadPlus (Parser s m)

instance (Monad m, Semigroup a) => Semigroup (Parser s m a) where
  p <> q = liftA2 (<>) p q

instance (Monad m, Semigroup a, Monoid a) => Monoid (Parser s m a) where
  mempty = return mempty
  mappend = (<>)
