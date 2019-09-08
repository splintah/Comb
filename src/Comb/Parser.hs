{-|
Module      : Comb.Parser
Description : Definition of 'Parser'
Copyright   : (C) Splinter Suidman, 2019
License     : AGPL-3

"Comb.Parser" contains the definition of a 'Parser' and re-exports combinators
for alternative parsing from the 'Alternative' type class in
"Control.Applicative".
-}
module Comb.Parser
  ( Parser (..)
  , ParserMonad (..)
    -- ** Re-exports
  , GreedyAlternative (..)
  , Alternative (..)
  ) where

import           Comb.Stream
import           Control.Applicative                   (Alternative (empty, (<|>)))
import           Control.Applicative.GreedyAlternative

-- | 'Parser' @m@ @s@ @a@ is a parser that uses 'Monad' @m@ to collect results,
-- reads from stream @s@, and returns a value of type @a@.
--
-- To provide instances of 'Functor', 'Applicative', 'Monad' and 'Alternative',
-- @m@ should have an instance of all these classes too. These classes can be
-- "summarised" with an instance of 'ParserMonad'.
newtype Parser m s a =
  Parser
    { runParser :: s -> m (a, s) -- ^ Run the parser on an input.
    }

instance (Functor f) => Functor (Parser f s) where
  fmap f (Parser px) = Parser $ \xs -> fmap (\(x, ys) -> (f x, ys)) (px xs)

instance (Monad f) => Applicative (Parser f s) where
  pure x = Parser $ \xs -> pure (x, xs)
  (Parser pf) <*> (Parser px) =
    Parser $ \fs -> do
      (f, xs) <- pf fs
      (x, ys) <- px xs
      pure (f x, ys)

instance (Monad f) => Monad (Parser f s) where
  (Parser px) >>= f =
    Parser $ \xs -> do
      (x, ys) <- px xs
      runParser (f x) ys

instance (Alternative f, Monad f) => Alternative (Parser f s) where
  empty = Parser $ const empty
  (Parser px) <|> (Parser py) = Parser $ \s -> px s <|> py s

instance (Monad m, GreedyAlternative m) => GreedyAlternative (Parser m s) where
  (Parser px) <||> (Parser py) = Parser $ \s -> px s <||> py s

-- | An empty class which requires 'Monad' and 'Alternative' instances mor @m@.
class (Monad m, Alternative m) => ParserMonad m

-- TODO: the following needs UndecidableInstances. Is it feasible to use?
-- instance (Monad m, Alternative m) => ParserMonad m
instance ParserMonad Maybe
instance ParserMonad []
