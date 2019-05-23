{-|
Module      : Comb.Stream
Description : Describes the interface of an input stream
Copyright   : (C) Splinter Suidman, 2019
License     : AGPL-3

"Comb.Stream" contains the 'Stream' typeclass with some instances. It describes
the interface of an input stream that can be used for parsing. A stream instance
of @s@ is not necessary to run a @Parser f s a@, but most primitive parsers in
"Comb.Prim" require one.
-}
{-# LANGUAGE UndecidableInstances #-}
module Comb.Stream
  ( Stream(..)
  ) where

import           Comb.Position
import           Data.Text     (Text)
import qualified Data.Text     as Text

-- | 'Stream' @s@ @t@ is a text stream of stream type @s@ with symbol type @t@.
class Stream s t | s -> t where
  -- | 'uncons' @s@ returns the first symbol in the stream together with the
  -- rest of the stream. If the end of the stream is reached, it should return
  -- 'Nothing'.
  uncons :: s -> Maybe (t, s)

instance Stream String Char where
  uncons xs =
    case xs of
      x:ys -> Just (x, ys)
      []   -> Nothing

instance Stream Text Char where
  uncons = Text.uncons

-- | A 'Stream' instance for 'WithPosition' @s@, provided @s@ is a stream of
-- @t@, and @t@ is an instance of 'UpdatePosition'.
instance (Stream s t, UpdatePosition t) => Stream (WithPosition s) t where
  uncons WithPosition { position, stream } = f <$> uncons stream
    where
      f (t, s) =
        (t, WithPosition { position = updatePosition t position, stream = s })
