{-|
Module      : Control.Applicative.GreedyAlternative
Description : Greedy version of 'Alternative'
Copyright   : (C) Splinter Suidman, 2019
License     : AGPL-3

"Control.Applicative.GreedyAlternative" defines the type class
'GreedyAlternative', which is like 'Alternative', but non-backtracking.
-}
module Control.Applicative.GreedyAlternative where

import           Control.Applicative (Alternative (empty, (<|>)))
import           Data.List.NonEmpty

-- | Greedy version of 'Alternative', which is used for non-backtracking
-- parsers.
class (Applicative f) => GreedyAlternative f where
  -- | A greedy version of '<|>'. '<||>' must return only the left-most
  -- success-value.
  --
  -- For 'Functor's like 'Maybe', '<||>' is no different from '<|>'; but for
  -- 'Functor's like @[]@, there is a difference, because the success value of
  -- @[]@ (a non-empty list) can contain multiple success values.
  (<||>) :: f a -> f a -> f a

instance GreedyAlternative Maybe where
  (<||>) = (<|>)

instance GreedyAlternative [] where
  (a:_) <||> _     = [a]
  []    <||> (b:_) = [b]
  []    <||> []    = []

instance GreedyAlternative NonEmpty where
  (a:|_) <||> _ = pure a

instance (Monoid e) => GreedyAlternative (Either e) where
  Right a <||> _       = Right a
  Left _  <||> Right b = Right b
  Left e  <||> Left f  = Left (e <> f)
