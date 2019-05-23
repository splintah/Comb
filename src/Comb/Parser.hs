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
  , ParserFunctor (..)
    -- ** Re-exports
  , Alternative (..)
  ) where

import Control.Applicative (Alternative (empty, (<|>)))

-- | 'Parser' @f@ @s@ @a@ is a parser that uses 'Functor' @f@ to collect
-- results, reads from stream @s@, and returns a value of type @a@.
--
-- To provide instances of 'Functor', 'Applicative', 'Monad' and 'Alternative',
-- @f@ should have an instance of all these classes too. These classes can be
-- "summarised" with an instance of 'ParserFunctor'.
newtype Parser f s a =
  Parser
    { runParser :: s -> f (a, s)
      -- ^ Run the parser on an input.
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

-- | An empty class which requires 'Monad' and 'Alternative' instances for @f@.
class (Monad f, Alternative f) => ParserFunctor f

-- TODO: the following needs UndecidableInstances. Is it feasible to use?
-- instance (Monad f, Alternative f) => ParserFunctor f
instance ParserFunctor Maybe
instance ParserFunctor []
