{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}

module Base.Editor (
  FixedFontEditor(..),
) where

import Base.Actions


class FixedFontEditor a c | a -> c where
  editText :: a -> EditAction c -> EditDirection -> a
  breakPara :: a -> EditDirection -> a
  moveCursor :: a -> MoveDirection -> a
  getCursor :: a -> (Int,Int)
