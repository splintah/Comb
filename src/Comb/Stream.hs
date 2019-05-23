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

-- -- | A stream 's' with symbol 's'.
-- class Stream s where
--   -- This approach allows for exactly one symbol type per stream type. If we had
--   -- used MultiParamTypeClasses, a stream could have two different symbols.
--   -- Which of these approaches is correct?
--   -- | The symbol which the stream contains.
--   type Symbol s :: *
--   -- | Update the position with the last-read character.
--   updatePos :: Position -> Symbol s -> Position
--   -- | Get the first symbol in the stream and the rest of the stream. If the
--   -- stream is empty, 'Nothing' must be returned.
--   uncons :: s -> Maybe (Symbol s, s)

-- updatePosChar :: Position -> Char -> Position
-- updatePosChar pos@Position { line, column } c =
--   case c of
--     '\n' -> pos { line = line + 1, column = 0 }
--     '\r' -> pos { column = 0 }
--     _ -> pos { column = column + 1 }

-- instance Stream String where
--   type Symbol String = Char
--   updatePos = updatePosChar
--   uncons cs =
--     case cs of
--       c:cs' -> Just (c, cs')
--       [] -> Nothing

-- instance Stream Text where
--   type Symbol Text = Char
--   updatePos = updatePosChar
--   uncons = Text.uncons
