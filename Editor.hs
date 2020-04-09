{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}

module Editor (
  FixedFontEditor(..),
) where

import Actions


class FixedFontEditor a c | a -> c where
  editText :: a -> EditAction c -> EditDirection -> a
  moveCursor :: a -> MoveDirection -> a
  getCursor :: a -> (Int,Int)