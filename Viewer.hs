{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}

module Viewer (
  FixedFontViewer(..),
) where


class FixedFontViewer a c | a -> c where
  setViewSize :: a -> (Int,Int) -> a
  getViewSize :: a -> (Int,Int)
  getVisible :: a -> [[c]]
