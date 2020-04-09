{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}

module Viewer (
  FixedFontViewer(..),
) where

import Actions
import Line


class FixedFontViewer a c | a -> c where
  shiftView :: a -> MoveDirection -> a
  setViewSize :: a -> (Int,Int) -> a
  getViewSize :: a -> (Int,Int)
  getVisible :: a -> [VisibleLine c]
