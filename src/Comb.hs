-- |Comb is a monadic parser combinator library for Haskell. The monadic context
-- (@m@ in the definition of @'Parser' s m a@) is generic; you can use a list,
-- 'Maybe', or any other type that is an instance of 'Monad' (and, for most
-- primitive combinators, 'MonadPlus'). When using lists, the entire parse
-- tree can be generated.
module Comb
  ( module Parser
  , module Primitives
  ) where

import Parser
import Primitives
